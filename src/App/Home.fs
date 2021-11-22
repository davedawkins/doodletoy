module Home

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
    | External of ExternalMessage

let init server =
    { Doodles = [] }, Cmd.ofMsg GetDoodles

let update (server : Server) dispatchExternal msg model =
    match msg with
    | External m ->
        dispatchExternal m
        model, Cmd.none
    | GetDoodles ->
        model,
        Cmd.OfPromise.result (server.AllDoodles() |> Promise.map (fun r -> r |> GotDoodles))
    | GotDoodles doodles ->
        //Fable.Core.JS.console.dir(turtles)
        { model with Doodles = doodles |> List.ofArray }, Cmd.none


let style = [
    rule ".home-turtle" [
        Css.fontSize (percent 75)
        Css.alignItemsCenter
        Css.width (px 200)
    ]
    //rule ".home-turtle:hover" [
    //    Css.fontWeightBold
    //]

    rule ".turtle-container" [
        Css.cursorPointer
        //Css.width (px 200)
        //Css.width (px 200)
    ]

    rule ".turtle-browser" [
        Css.displayGrid
        Css.gap (rem 2)
        Css.custom("justifyItems", "center")
        Css.custom("grid-template-columns", "repeat(auto-fill, minmax(200px, 1fr))")
    ]
]

let nameOf (email : string) =
    email.Split('@').[0]

let fa name attrs = Html.i [ Attr.className (sprintf "fa fa-%s" name); yield! attrs ]
module DoodleView =
    type Model = {
        Doodle : Schema.Doodle
        View : Schema.DoodleView option
    }

    type Message =
        | GetView
        | GotView of Schema.DoodleView
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


    let private viewRecord dispatch dispatchLocal (m : Model) =
        let make() = Turtle.drawTurtle m.Doodle.source ()
        let drawingS = Store.make (make())
        let refresh() =
            Store.set drawingS (make())
        let mutable stopAnimate = ignore
        let startAnimate() =
            stopAnimate <- DOM.interval refresh 40

        UI.flexColumn [
            Attr.className "home-turtle"

            UI.divc "turtle-container" [
                Turtle.turtleView drawingS
                Ev.onClick (fun _ -> EditTurtle m.Doodle |> External |> dispatchLocal)
                Ev.onMouseEnter (fun _ -> startAnimate())
                Ev.onMouseLeave (fun _ -> stopAnimate())
            ]

            UI.flexRow[
                Attr.style [
                    Css.justifyContentCenter
                    Css.width (percent 90)
                    Css.gap (rem 1)
                ]
                UI.flexColumn [
                    Attr.style [
                        Css.flexGrow 1
                    ]
                    Html.span m.Doodle.name
                    Html.span (sprintf "by %s" m.Doodle.ownedByName)
                ]
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

    let view server dispatchLocal (doodle : Schema.Doodle) =
        let model, dispatch = doodle |> Store.makeElmish (init server) (update server) ignore
        Bind.el(model, viewRecord dispatch dispatchLocal)

let view (server : Server) (dispatchExternal) =
    let model, dispatch = server |> Store.makeElmish init (update server dispatchExternal) ignore
    let featured() = Turtle.drawTurtle Examples.clockSource ()
    let drawingStore = Turtle.Ticker.Create 40 featured (fun _ _ -> featured()) ignore

    Html.div [
        DOM.disposeOnUnmount [drawingStore]
        Attr.className "hero"
        //text "Featured Turtle"
        //Turtle.turtleView drawingStore
        UI.divc "turtle-browser" [
            Bind.each( model |> Store.map (fun m -> m.Doodles), DoodleView.view server dispatch, (fun r -> r._id) )
        ] |> withStyle style
    ]

let nav (server : Server) (dispatch)=
    Bind.el( server.User, fun user ->
        match user with
        | None -> fragment []
        | Some u ->
            UI.navItem "New" (fun _ -> dispatch NewTurtle)
    )

