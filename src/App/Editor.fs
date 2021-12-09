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
        match isNullOrUndefined (id) with
        | true -> "_new_"
        | false -> id

    let getDoodles() : obj =
        match localStorage.getItem("doodles") with
        | null -> upcast {| |}
        | x -> Fable.Core.JS.JSON.parse x

    let setDoodles( map : obj ) =
        localStorage.setItem("doodles", JSON.stringify map)

        Fable.Core.JS.console.log("== Doodles ==")
        Fable.Core.JS.console.dir(getDoodles())

    let load( id : string ) : Schema.Doodle option =
        let id' = safeId id
        let map = getDoodles()
        match isIn id' map with
        | false -> None
        | true -> map?(id') |> Some

    let clear (id : string) : unit =
        setDoodles {| |}

    let save (d : Schema.Doodle) =
        let map = {| |}
        map?current <- d
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
    | ClearLocal of string
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
    let doodle', edited' =
        match backup with
        | Some d when d._id = doodle._id && d.modifiedOn = doodle.modifiedOn -> d, true
        | _ -> doodle, false

    {
        TickCount = 0
        IsEdited = edited'
        Doodle = doodle'
        Mouse = (0.0,0.0)
        AnimationEnabled = false
    },
    //Cmd.none
    [ fun d -> Fable.Core.JS.setInterval (fun _ -> d Tick) 40 |> ignore ]

let update (server : Server) (session:DoodleSession option) msg (model : Model)=
    //Fable.Core.JS.console.log($"{msg}")
    match msg with
    | Mouse (x,y) -> { model with Mouse = (x,y) }, Cmd.none
    | Init d ->
        { model with Doodle = d; IsEdited = false }, Cmd.none
    | Error x -> model, Cmd.none
    | Saved d ->
        let _cacheId = model.Doodle._id
        //Fable.Core.JS.console.log($"ID after save is {d._id}")
        { model with Doodle = d; IsEdited = false }, Cmd.ofMsg (ClearLocal _cacheId)
    | Discard ->
        let _cacheId = model.Doodle._id
        let cmd =
            if Fable.Core.JsInterop.isNullOrUndefined model.Doodle._id then
                Schema.Doodle.Create() |> Init |> Cmd.ofMsg
            else
                let d = server.GetDoodle( model.Doodle._id )
                Cmd.OfPromise.result (d |> Promise.map Init)
        model, Cmd.batch [ cmd; Cmd.ofMsg SaveLocal; Cmd.ofMsg (ClearLocal _cacheId) ]
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
        { model with Doodle = { model.Doodle with name = s }; IsEdited = true }, Cmd.ofMsg SaveLocal
    | SetDescription s ->
        { model with Doodle = { model.Doodle with description = s }; IsEdited = true }, Cmd.ofMsg SaveLocal
    | SetSource s ->
        { model with Doodle = { model.Doodle with source = s }; IsEdited = true }, Cmd.ofMsg SaveLocal
    | ClearLocal id ->
        Storage.clear id
        model, Cmd.none
    | SaveLocal ->
        Storage.save model.Doodle
        model, Cmd.none
    | Tick ->
        { model with TickCount = model.TickCount + 1}, Cmd.none

let style = [
    rule ".turtle-details" [
        Css.gap (rem 0.5)
        Css.flexShrink 1
        Css.flexGrow 1
        Css.maxWidth (vh 65)
    ]
    rule "label" [
        Css.width (px 100)
    ]
    rule ".turtle-details input" [
        Css.flexGrow 1
    ]
    rule ".turtle-details textarea" [
        Css.flexGrow 1
        Css.height (rem 5)
    ]
    rule ".turtle-details .buttons" [
        Css.gap (rem 1)
        Css.justifyContentCenter
    ]

    rule ".turtle-view" [
        Css.width (percent 100)
    ]

    rule ".turtle-editor" [
        Css.flexGrow 1
        Css.flexShrink 1
        Css.width (px 500)
        Css.height (px 710)
        Css.minWidth (px 400)
        Css.maxWidth (px 700)
        Css.backgroundColor "#202020"
        Css.color "beige"
        Css.padding (rem 1)
        Css.fontFamily "Consolas, monospace"
        Css.fontSize (pt 10)
    ]

]

let turtleView (dispatch : (float*float) -> unit) turtle =
    Html.div [
        Attr.className "turtle-view"
        Attr.style [
            Css.flexGrow 1
            Css.custom ("aspect-ratio", "1 / 1")
        ]
        DrawingCanvas [
            Attr.style [
                Css.width (percent 100)
                Css.height (percent 100)
                Css.borderWidth  (px 1)
                Css.borderStyle Feliz.borderStyle.solid
                Css.borderColor "#cccdcc"
                Css.borderRadius (px 10)
            ]
        ] {
            Drawing = turtle
            //Redraw = ignore
            OnMouseMove = Some dispatch }
    ]

let _view readonly model dispatch =
    let mutable mx = 0.0
    let mutable my = 0.0

    UI.flexRow [

        Attr.style [
            Css.justifyContentSpaceBetween
            Css.gap (rem 4)
            Css.custom("align-items", "start")
        ]

        UI.flexColumn [
            Attr.className "turtle-details"

            model |> Store.map turtleFromModel |> (turtleView (Mouse>>dispatch))

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
                UI.flexRow [
                    Attr.className "buttons"
                    Html.button [
                        Bind.el(model |> Store.map (fun m -> m.IsEdited),
                            function true -> text "Save *" | false -> text "Save"
                        )
                        Ev.onClick (fun _ -> dispatch Save)
                    ]
                    Html.button [
                        text "Save as New"
                        Ev.onClick (fun _ -> dispatch SaveAsNew)
                    ]
                    Bind.visibility (model |> Store.map (fun m -> m.IsEdited)) <|
                        Html.button [
                            text "Discard"
                            Ev.onClick (fun _ -> dispatch Discard)
                        ]
                ]
        ]

        Html.textarea [
            Attr.className  "turtle-editor"
            Bind.attr("value", model .> (fun m -> m.Doodle.source), dispatch << SetSource)
        ]
    ] |> withStyle style

let view (server : Server) (session : DoodleSession option) doodle =
    let model, dispatch = doodle |> Store.makeElmish init (update server session) ignore
    _view (session.IsNone) model dispatch
