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


type SortMode =
    | ByLastModified
    | ByNumberOfLikes
    | ByNumberOfViews
    | ByAuthor
type Model =
    {
        Doodles : DoodleView array
        SortMode : SortMode
    }


type Message =
    | SetSort of SortMode
    | GetDoodles
    | GotDoodles of DoodleView[]

let init server =
    { Doodles = [| |]; SortMode = ByLastModified }, Cmd.ofMsg GetDoodles

let update (server : Server) msg model =
    match msg with
    | SetSort mode ->
        { model with SortMode = mode }, Cmd.none
    | GetDoodles ->
        model,
        Cmd.OfPromise.result (server.AllDoodleViews() |> Promise.map (fun r -> r |> GotDoodles))
    | GotDoodles doodles ->
        //Fable.Core.JS.console.dir(turtles)
        { model with Doodles = doodles }, Cmd.none


let nameOf (email : string) =
    email.Split('@').[0]

let fa name attrs = Html.i [ Attr.className (sprintf "fa fa-%s" name); yield! attrs ]

let numViews (views : Schema.Views array) = views |> Array.fold (fun n v -> n + int(v.numViews)) 0
let numLikes (likes : Schema.Like array) = likes.Length

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
        Doodle : DoodleView
        //View : DoodleView option
    }

    type private Message =
        | GetView
        | GotView of DoodleView
        | ToggleLike
        | Error of System.Exception

    let private init (server : Server) d : Model * Cmd<Message> =
        {
            Doodle = d
            //View = None
        }, Cmd.none //Cmd.OfPromise.result (server.GetDoodleView(d) |> Promise.map GotView)

    let private update (server : Server) (msg : Message) (model : Model) : Model * Cmd<Message>=
        match msg with
        | Error x ->
            Fable.Core.JS.console.error(x.Message)
            model, Cmd.none
        | ToggleLike ->
            model, Cmd.OfPromise.either server.ToggleLike (model.Doodle.Doodle) (fun _ -> GetView) Error
        | GetView ->
            model, Cmd.OfPromise.result (server.GetDoodleView(model.Doodle.Doodle) |> Promise.map GotView)
        | GotView v -> { model with Doodle = v }, Cmd.none

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
        let make() = Editor.drawTurtle m.Doodle.Doodle.source mousexy
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
                Ev.onClick (fun _ -> Browser.Dom.window.location.href <- "#edit?d=" + m.Doodle.Doodle._id)

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
                        Html.span m.Doodle.Doodle.name
                        Html.span (sprintf "by %s" m.Doodle.Doodle.ownedByName)
                    ]
                    Bind.el(server.State, fun state ->
                        let isAdmin = state.User |> Option.map (fun u -> u.IsAdmin) |> Option.defaultValue false
                        let isFeatured = state.Configuration.featured = m.Doodle.Doodle._id

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
                                            Ev.onClick (fun _ -> server.SetFeatured(m.Doodle.Doodle) |> ignore )
                                        ]
                                ]
                            ]
                        else
                            fragment [ ]

                    )
                    Html.div [
                        fa "eye" []
                        text (sprintf " %d" (numViews(m.Doodle.Views)))
                    ]
                    Html.div [
                        let heartIcon =
                            match m.Doodle.MyLike with
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

                        text (sprintf " %d" (numLikes(m.Doodle.Likes)))
                    ]
                ]
            ]

    let view server (options : Options) (doodle : DoodleView) =
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

    rule ".sort-buttons" [
        Css.displayFlex
        Css.flexDirectionRow
        Css.gap (rem 1)
        Css.marginBottom (rem 1)
    ]

    rule ".sort-buttons a.active" [
        Css.fontWeightBold
    ]
]

open DoodleView

let sorter sortMode : (DoodleView -> DoodleView -> int) =
    let compareByViews a b = numViews(b.Views) - numViews(a.Views)

    match sortMode with
    | ByAuthor -> fun a b -> a.Doodle.ownedByName.CompareTo(b.Doodle.ownedByName)

    | ByNumberOfLikes -> fun a b ->
            let na = numLikes(a.Likes)
            let nb = numLikes(b.Likes)
            if na = nb then compareByViews a b else nb - na

    | ByNumberOfViews -> compareByViews

    | ByLastModified -> fun a b -> System.Math.Sign( b.Doodle.modifiedOn - a.Doodle.modifiedOn )

let sortDoodles (doodles: DoodleView array) sortMode =
    doodles |> Array.sortWith (sorter sortMode) |> Array.toList

let view (server : Server) =
    let model, dispatch = server |> Store.makeElmish init (update server) ignore

    let options = {
        Animation = Hover
        SizePx = 250
        ShowFooter = true
    }

    let sortButton label sortMode currentMode =
        Html.a [
            Attr.href "#"
            if sortMode = currentMode then
                Attr.className "active"
            text label
            Ev.onClick (fun e -> e.preventDefault(); dispatch (SetSort sortMode))
        ]

    Html.div [
        Attr.className "browse"
        UI.divc "explain" [
            Html.p "Hover over a doodle to activate it; it may animate and may also respond to the mouse location. "
            Html.p "Click on a doodle to see how it works and create your own version of it"
        ]

        Bind.el(model |> Store.map (fun m -> m.SortMode), fun sortMode ->
            UI.divc "sort-buttons" [
                Html.span "Sort by:"
                sortButton "Recent" ByLastModified sortMode
                sortButton "Popular" ByNumberOfLikes sortMode
                sortButton "Views" ByNumberOfViews sortMode
                sortButton "Author" ByAuthor sortMode
            ])

        Bind.el(model |> Store.map (fun m -> m.SortMode), fun sortMode ->
            UI.divc "turtle-browser" [
                Bind.each( model |> Store.map (fun m -> sortDoodles m.Doodles sortMode), DoodleView.view server options, (fun r -> r.Doodle._id) )
            ]
        )
    ] |> withStyle style
