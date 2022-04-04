module Editor

open Sutil
open Server
open UI
open Fable.DrawingCanvas
open Fable.DrawingCanvas.Builder
open type Feliz.length
open Sutil.DOM
open Types
open System
open Sutil.Styling
open AppwriteSdk
open Types.Schema

let private _log (s:string) = Fable.Core.JS.console.log(s)

module Ticker =

    let Create<'T> (interval : int) (init : unit -> 'T) (value : int -> 'T -> 'T) (dispose : 'T -> unit)=
        let mutable stop : unit -> unit = ignore
        let mutable tick : int = 0
        let s = ObservableStore.makeStore init (fun v -> stop(); dispose v)
        stop <- DOM.interval
            (fun _ ->
                tick <- tick + 1
                s |> Store.modify (value tick)
                )
            interval
        s

module Storage =

    open Fable.Core.JsInterop
    open Fable.Core.JS
    open Fable.Core

    let localStorage = Browser.Dom.window.localStorage

    [<Emit("delete $1[$0]")>]
    let delete id map : unit = jsNative

    let safeId (id : string) =
        match Server.IsValidId id with
        | false -> "_new_"
        | true -> id

    let getDoodles() : obj =
        match localStorage.getItem("doodles") with
        | null -> upcast {| |}
        | x -> Fable.Core.JS.JSON.parse x

    let setDoodles( map : obj ) =
        localStorage.setItem("doodles", JSON.stringify map)

        // Fable.Core.JS.console.log("== Doodles ==")
        // Fable.Core.JS.console.dir(getDoodles())

    let load( id : string ) : Schema.Doodle option =
        let id' = safeId id
        let map = getDoodles()
        match isIn id' map with
        | false -> None
        | true -> map?(id') |> Some

    let clear () : unit =
        setDoodles {| |}

    let save (d : Schema.Doodle) =
        let map = {| |}
        map?current <- d.UpdateModifiedOn(Server.DateTimeNow)
        setDoodles map

    let get() : Schema.Doodle option =
        load "current"

type Model = {
    Doodle : Types.Schema.Doodle
    TickCount : int
    IsEdited : bool
    Mouse: float * float
    AnimationEnabled : bool
}

type Message =
    | Init of Schema.Doodle
    | SetName of string
    | SetDescription of string
    | SetSource of string
    | Tick
    | Discard
    | Save
    | SaveAsNew
    | SaveLocal
    | ClearLocal
    | Saved of Schema.Doodle
    | Error of Exception
    | Mouse of (float*float)

let drawMessage msg color = drawing {
    save
    fillColor "white"
    fillRect -500. -500. 1000. 1000.

    font "32px arial"
    fillColor color
    textAlign "center"
    fillText msg 0.0 0.0 9999.0
    restore
}

let drawMessageImmediate msg color = fun canvas ->
    let drawing = drawMessage msg color
    initContext canvas
    canvas.save()
    drawing() |> (canvas |> runCommands)
    canvas.restore()

let drawTurtle (source :string) ((mx,my) : (float*float)) =
    let input = source.Trim()
    match input with
    | "" -> drawMessageImmediate "Create your own drawing, or load an example to get started" "black"
    | _ ->
        try
            let vars =
                Map.empty
                    .Add("mx", TurtleParser.Float mx)
                    .Add("my", TurtleParser.Float my)
                    ;

            match (TurtleParser.generateRedraw input vars) with
            // Error messages from crude parser are currently useless
            | TurtleParser.Parser.ParseResult.Error msg -> drawMessageImmediate $"Oops! Not a turtle: {msg}" "orange"
            | TurtleParser.Parser.ParseResult.Success d -> d
        with
        | x -> drawMessageImmediate $"Oops! Not a turtle: {x.Message}" "orange"

let turtleFromModel model = (drawTurtle model.Doodle.source model.Mouse)

let init (doodle : Schema.Doodle) =
    //Fable.Core.JS.console.log("ID=" + doodle._id)
    let backup =
        try
            Storage.get()
        with x ->
            Fable.Core.JS.console.log(x.Message)
            None

    let edited' =
        match backup with
        | Some d when d._id = doodle._id -> true
        | _ -> false

    {
        TickCount = 0
        IsEdited = edited'
        Doodle = doodle
        Mouse = (0.0,0.0)
        AnimationEnabled = false
    },
    //Cmd.none
    [ fun d -> Fable.Core.JS.setInterval (fun _ -> d Tick) 40 |> ignore ]

let update (server : Server) (session:DoodleSession option) msg (model : Model)=
    match msg with
    | Mouse (x,y) -> { model with Mouse = (x,y) }, Cmd.none
    | Init d ->
        { model with Doodle = d; IsEdited = false }, Cmd.none
    | Error x -> model, Cmd.none
    | Saved newDoodle ->
        let _cacheId = model.Doodle._id
        { model with Doodle = newDoodle; IsEdited = false },
            Cmd.batch [
                Cmd.ofMsg (ClearLocal)
                [(fun d -> if _cacheId <> newDoodle._id then Browser.Dom.window.location.href <- "#edit?d=" + newDoodle._id)]
                ]
    | Discard ->
        let _cacheId = model.Doodle._id
        let cmd =
            if not(Server.IsValidId model.Doodle._id) then
                Schema.Doodle.Create() |> Init |> Cmd.ofMsg
            else
                let d = server.GetDoodle( model.Doodle._id )
                Cmd.OfPromise.result (d |> Promise.map Init)
        model, Cmd.batch [ cmd; Cmd.ofMsg SaveLocal; Cmd.ofMsg (ClearLocal) ]
    | Save ->
        match session with
        | Some s ->
            model, Cmd.OfPromise.either (s.Save) (model.Doodle) Saved Error
        | None -> model, Cmd.none
    | SaveAsNew ->
        match session with
        | Some s ->
            model, Cmd.OfPromise.either (s.SaveAsNew) (model.Doodle) Saved Error
        | None -> model, Cmd.none
    | SetName s ->
        { model with Doodle = model.Doodle.UpdateName(name = s); IsEdited = true }, Cmd.ofMsg SaveLocal
    | SetDescription s ->
        { model with Doodle = model.Doodle.UpdateDescription( description = s ); IsEdited = true }, Cmd.ofMsg SaveLocal
    | SetSource s ->
        { model with Doodle = model.Doodle.UpdateSource( source = s ); IsEdited = true }, Cmd.ofMsg SaveLocal
    | ClearLocal ->
        Storage.clear()
        model, Cmd.none
    | SaveLocal ->
        Storage.save model.Doodle
        model, Cmd.none
    | Tick ->
        { model with TickCount = model.TickCount + 1}, Cmd.none

let style = [

    rule ".editor-container" [
        Css.displayGrid
        Css.gap (rem 2)
    ]

    Media.MinWidth( UI.BreakPoint, [
        rule ".editor-container" [
            Css.displayFlex
            Css.flexDirectionRow
            Css.justifyContentCenter
            Css.alignItemsFlexStart
        ]
    ])

    rule ".doodle-name-description" [
        Css.gap (rem 0.5)
        Css.flexShrink 1
        Css.flexGrow 1
        Css.maxWidth (vh 65)
    ]

    rule ".doodle-name-description input" [
        Css.flexGrow 1
    ]

    rule ".doodle-name-description textarea" [
        Css.flexGrow 1
        Css.height (rem 5)
    ]
    rule ".doodle-name-description .buttons" [
        Css.gap (rem 1)
        Css.justifyContentCenter
    ]

    rule ".doodle-name-description label" [
        Css.width (px 100)
    ]

    rule ".doodle-canvas-container" [
        Css.width (percent 100)
    ]

    rule ".doodle-editor" [
        Css.backgroundColor "#202020"
        Css.color "beige"
        Css.padding (rem 1)
        Css.fontFamily "Consolas, monospace"
        Css.fontSize (pt 10)
        Css.custom("tab-size", "4")
        Css.flexGrow 1
        Css.flexShrink 1
        Css.height (px 710)
    ]

    Media.MinWidth( UI.BreakPoint,[
        rule ".doodle-editor" [
            Css.width (px 500)
            Css.minWidth (px 400)
            Css.maxWidth (px 700)
        ]
    ])
]

let drawingCanvas items props =
    DrawingCanvas items props
    //UI.divc "test-canvas" items

let doodleCanvasContainer (dispatch : (float*float) -> unit) doodle =
    Html.div [
        Attr.className "doodle-canvas-container"
        Attr.style [
            Css.flexGrow 1
            Css.custom ("aspect-ratio", "1 / 1")
        ]
        drawingCanvas [
            Attr.style [
                Css.width (percent 100)
                Css.height (percent 100)
                Css.borderWidth  (px 1)
                Css.borderStyle Feliz.borderStyle.solid
                Css.borderColor "#cccdcc"
                Css.borderRadius (px 10)
            ]
        ] {
            Drawing = doodle
            OnMouseMove = Some dispatch }
    ]

let supportInsertTab dispatch (e : Browser.Types.KeyboardEvent) =
    if e.key = "Tab" && not e.ctrlKey then
        let el = e.target :?> Browser.Types.HTMLTextAreaElement
        e.preventDefault()
        let value, selstart, selend = el.value, el.selectionStart, el.selectionEnd

        // set textarea value to: text before caret + tab + text after caret
        dispatch (SetSource  (value.Substring(0, selstart) + "\t" + value.Substring(selend)))

        // put caret at right position again
        el.selectionStart <- selstart + 1
        el.selectionEnd <- selstart + 1

let _view (session : DoodleSession option) model dispatch =
    let mutable mx = 0.0
    let mutable my = 0.0
    let readonly = session.IsNone

    let isMyDoodle (d : Schema.Doodle) =
        match session with
        | Some s -> s.User._id = d.ownedBy
        | _ -> false

    let isNewDoodle (d : Schema.Doodle) =
        isNull(d._id)

    // Details | TextArea
    UI.divc "editor-container" [

        // Doodle
        // Name+Description+Buttons
        UI.grid [
            Attr.className "doodle-name-description"

            model |> Store.map turtleFromModel |> (doodleCanvasContainer (Mouse>>dispatch))

            UI.flexRow [
                Html.label [ text "Name:" ]

                Html.input [
                    if readonly then
                        Attr.readOnly true
                    Bind.attr("value", model |> Store.map (fun m -> m.Doodle.name), dispatch<<SetName)
                ]
            ]
            UI.flexRow [
                Html.label [ text "Description:" ]
                Html.textarea [
                    if readonly then
                        Attr.readOnly true
                    Bind.attr("value", model |> Store.map (fun m -> m.Doodle.description), dispatch<<SetDescription)
                ]
            ]

            if not readonly then

                Bind.el( model |> Store.map (fun m -> m.Doodle, m.IsEdited) |> Observable.distinctUntilChanged , fun (doodle,isEdited) ->
                    let isNew = isNull(doodle._id)

                    UI.flexRow [
                        if isEdited then
                            Html.span "Doodle has been modified: "

                        Attr.className "buttons"

                        if isEdited && not (isNewDoodle doodle) && isMyDoodle doodle then
                            Html.button [
                                text ("Save Changes")
                                Ev.onClick (fun _ -> dispatch Save)
                            ]

                        Html.button [
                            text (if isNew then "Save as New" else "Save as Copy")
                            Ev.onClick (fun _ -> dispatch SaveAsNew)
                        ]

                        if isEdited then
                            Html.button [
                                text "Discard Changes"
                                Ev.onClick (fun _ -> _log("discard"); dispatch Discard)
                            ]

                        if (not isEdited) && not(isNull(doodle._id)) then
                            Html.button [
                                text ("Share")
                                Ev.onClick (fun _ -> Browser.Dom.window.location.href <- "#view?d=" + doodle._id)
                            ]
                ])
            else
                UI.flexRow [
                    Html.p [
                        Attr.className "suggest-signin"
                        Html.a [
                            Attr.href "#signin"
                            text "Sign in"
                        ]
                        text " to be able to save your doodles!"
                    ]
                ]
        ]

        Html.textarea [
            Attr.className  "doodle-editor"
            Bind.attr("value", model .> (fun m -> m.Doodle.source), dispatch << SetSource)
            Ev.onKeyDown (supportInsertTab dispatch)
        ]
    ] |> withStyle style

let view (server : Server) (session : DoodleSession option) doodle =
    let model, dispatch = doodle |> Store.makeElmish init (update server session) ignore
    _view session model dispatch
