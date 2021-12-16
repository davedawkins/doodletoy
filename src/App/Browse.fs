module Browse

open Sutil
open Sutil.DOM
open Sutil.Bindings
open Server
open Types
open UI
open Sutil.Styling
open type Feliz.length
open AppwriteSdk

type Model =
    {
        Doodles : Schema.Doodle list
    }
type Message =
    | GetDoodles
    | GotDoodles of Schema.Doodle[]

let init server =
    { Doodles = [] }, Cmd.ofMsg GetDoodles

let update (server : Server) msg model =
    match msg with
    | GetDoodles ->
        model,
        Cmd.OfPromise.result (server.AllDoodles() |> Promise.map (fun r -> r |> GotDoodles))
    | GotDoodles doodles ->
        //Fable.Core.JS.console.dir(turtles)
        { model with Doodles = doodles |> List.ofArray }, Cmd.none


let nameOf (email : string) =
    email.Split('@').[0]

let fa name attrs = Html.i [ Attr.className (sprintf "fa fa-%s" name); yield! attrs ]

module DoodleView =

    type AnimationMode =
        | Never
        | Hover
        | Always

    type Options = {
        Animation : AnimationMode
        SizePx : int
        ShowFooter : bool
    }

    type private Model = {
        Doodle : Schema.Doodle
        View : DoodleView option
    }

    type private Message =
        | GetView
        | GotView of DoodleView
        | ToggleLike
        | Error of System.Exception

    let private init (server : Server) d : Model * Cmd<Message> =
        {
            Doodle = d
            View = None
        }, Cmd.OfPromise.result (server.GetDoodleView(d) |> Promise.map GotView)

    let private update (server : Server) (msg : Message) (model : Model) : Model * Cmd<Message>=
        match msg with
        | Error x ->
            Fable.Core.JS.console.error(x.Message)
            model, Cmd.none
        | ToggleLike ->
            model, Cmd.OfPromise.either server.ToggleLike (model.Doodle) (fun _ -> GetView) Error
        | GetView ->
            model, Cmd.OfPromise.result (server.GetDoodleView(model.Doodle) |> Promise.map GotView)
        | GotView v -> { model with View = Some v }, Cmd.none

    let private style(options : Options) = [
        rule ".doodle-card" [
            Css.displayFlex
            Css.flexDirectionColumn
            Css.fontSize (percent 75)
            Css.alignItemsCenter
            Css.margin(px 0, auto)
        ]

        Media.MinWidth( UI.BreakPoint, [
            rule ".doodle-card" [
                Css.width (vh 75)
            ]
            rule ".doodle-card.sizepx" [
                Css.width (px options.SizePx)
            ]
        ])
        rule ".doodle-view" [
            Css.cursorPointer
            Css.width (percent 100)
        ]

        rule ".fa-star" [
            Css.color "#ff009b"
        ]

        rule ".doodle-details" [
            Css.displayFlex
            Css.flexDirectionRow
            Css.justifyContentCenter
            Css.width (percent 90)
            Css.gap (rem 1)
        ]
    ]

    let private viewRecord (server:Server)  (options : Options) dispatch (m : Model) =
        let mutable mousexy : float * float = 0.0,0.0
        let make() = Editor.drawTurtle m.Doodle.source mousexy
        let drawingS = Store.make (make())
        let refresh() =
            Store.set drawingS (make())
        let mutable stopAnimate = ignore
        let startAnimate() =
            stopAnimate <- DOM.interval refresh 40

        if options.Animation = Always then startAnimate()

        let sizeClass = if options.SizePx > 0 then " sizepx" else ""
        UI.divc ("doodle-card" + sizeClass) [

            unsubscribeOnUnmount [ stopAnimate ]
            disposeOnUnmount [ drawingS ]

            UI.divc "doodle-view" [
                Editor.doodleCanvasContainer (fun mxy -> mousexy <- mxy) drawingS
                Ev.onClick (fun _ -> Browser.Dom.window.location.href <- "#edit?d=" + m.Doodle._id)

                if options.Animation = Hover then
                    Ev.onMouseEnter (fun _ -> startAnimate())
                    Ev.onMouseLeave (fun _ -> stopAnimate())
            ]

            if options.ShowFooter then
                UI.divc "doodle-details" [
                    UI.flexColumn [
                        Attr.style [
                            Css.flexGrow 1
                        ]
                        Html.span m.Doodle.name
                        Html.span (sprintf "by %s" m.Doodle.ownedByName)
                    ]
                    Bind.el(server.State, fun state ->
                        let isAdmin = state.User |> Option.map (fun u -> u.IsAdmin) |> Option.defaultValue false
                        let isFeatured = state.Configuration.featured = m.Doodle._id

                        if isFeatured || isAdmin then
                            let canSelect = not isFeatured
                            Html.div [
                                Html.i [
                                    Attr.classes [
                                        "fa"
                                        match isFeatured with true->"fa-star"|false->"fa-star-o"
                                    ]
                                    if canSelect then
                                        yield! [
                                            Attr.style [ Css.cursorPointer ]
                                            Ev.onClick (fun _ -> server.SetFeatured(m.Doodle) |> ignore )
                                        ]
                                ]
                            ]
                        else
                            fragment [ ]

                    )
                    Html.div [
                        fa "eye" []
                        m.View
                        |> Option.map (fun v ->
                            text (sprintf " %d" (v.Views |> Array.fold (fun n v -> n + int(v.numViews)) 0))
                        )
                        |> Option.defaultValue (text " 0")
                    ]
                    Html.div [
                        let heartIcon =
                            match m.View |> Option.bind(fun v -> v.MyLike) with
                            |None ->
                                "heart-o"
                            |Some like ->
                                "heart"

                        fa heartIcon [
                            Attr.style [
                                Css.cursorPointer
                            ]
                            Ev.onClick (fun _ -> dispatch ToggleLike)
                        ]

                        m.View
                        |> Option.map (fun v ->
                            text (sprintf " %d" v.Likes.Length)
                        )
                        |> Option.defaultValue (text " 0")
                    ]
                ]
            ]

    let view server (options : Options) (doodle : Schema.Doodle) =
        let model, dispatch = doodle |> Store.makeElmish (init server) (update server) ignore
        Bind.el(model, viewRecord server options dispatch) |> withStyle (style options)


let style = [

    rule ".turtle-browser" [
        Css.displayGrid
        Css.gap (rem 2)
        Css.custom("justifyItems", "center")
        Css.custom("grid-template-columns", "repeat(auto-fill, minmax(250px, 1fr))")
    ]

    rule "div.explain" [
        Css.marginBottom (rem 2)
    ]
    rule ".explain>p" [
        Css.lineHeight (rem 1.5)
        Css.marginTop (rem 0.5)
        Css.marginBottom (rem 0.5)
    ]

]

open DoodleView

let view (server : Server) =
    let model, dispatch = server |> Store.makeElmish init (update server) ignore

    let options = {
        Animation = Hover
        SizePx = 250
        ShowFooter = true
    }

    Html.div [
        Attr.className "browse"
        UI.divc "explain" [
            Html.p "Hover over a doodle to activate it; it may animate and may also respond to the mouse location. "
            Html.p "Click on a doodle to see how it works and create your own version of it"
        ]
        UI.divc "turtle-browser" [
            Bind.each( model |> Store.map (fun m -> m.Doodles), DoodleView.view server options, (fun r -> r._id) )
        ]
    ] |> withStyle style
