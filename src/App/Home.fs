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
        Featured : Schema.Doodle option
    }
type Message =
    | GetFeatured of string
    | SetFeatured of Schema.Doodle option
    | External of ExternalMessage

let init (server : Server) =
    { Featured = None }, Cmd.none

let update (server : Server) msg model =
    match msg with
    | External m ->
        server.Dispatch m
        model, Cmd.none
    | SetFeatured d ->
        { model with Featured = d}, Cmd.none
    | GetFeatured id->
        model,
        Cmd.OfPromise.result (server.GetDoodle(id) |> Promise.map (fun r -> r |> (SetFeatured << Some)))


let style = [
    rule ".home-turtle" [
        Css.fontSize (percent 75)
        Css.alignItemsCenter
        Css.width (px 250)
    ]

    rule ".turtle-container" [
        Css.cursorPointer
    ]

    rule ".turtle-browser" [
        Css.displayGrid
        Css.gap (rem 2)
        Css.custom("justifyItems", "center")
        Css.custom("grid-template-columns", "repeat(auto-fill, minmax(250px, 1fr))")
    ]

    rule ".featured" [
        Css.maxWidth (vh 50)
    ]
]

open Browse.DoodleView

let view (server : Server) =
    let model, dispatch = server |> Store.makeElmish init (update server) ignore

    let unsub = (server.State |> Store.map ( fun s -> s.Configuration.featured)).Subscribe( fun f ->
        match f with
        | "" -> ()
        | id -> id |> GetFeatured |> dispatch
    )

    Html.div [
        disposeOnUnmount [ unsub ]
        Html.h2 "Featured Doodle"
        Bind.el( model |> Store.map (fun m -> m.Featured), fun doodleOpt ->
            let options = { Animation = Always; SizePx = 400 }
            match doodleOpt with
            | Some doodle ->
                Browse.DoodleView.view server options doodle
            | None -> fragment []
            // let source = doodle |> Option.map (fun d -> d.source) |> Option.defaultValue (Examples.clockSource)
            // let featured() = Turtle.drawTurtle source ()
            // let drawingStore = Turtle.Ticker.Create 40 featured (fun _ _ -> featured()) ignore
            // Html.div [
            //     Attr.className "featured"
            //     DOM.disposeOnUnmount [drawingStore]
            //     Turtle.turtleView drawingStore
            // ]
        )
    ] |> withStyle style

let nav (server : Server) (dispatch)=
    Bind.el( server.State |> Store.map (fun s -> s.User), fun user ->
        match user with
        | None -> fragment []
        | Some u ->
            UI.navItem "New" (fun _ -> dispatch NewTurtle)
    )

