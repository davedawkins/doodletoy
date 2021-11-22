module Turtle

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

type Model = {
    Doodle : Types.Schema.Doodle
    TickCount : int
    IsEdited : bool
}

type Message =
    | SetName of string
    | SetDescription of string
    | SetSource of string
    | Tick
    | Save
    | Saved of Schema.Doodle
    | Error of Exception

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

let drawTurtle (source :string) =
    let input = source.Trim()
    match input with
    | "" -> drawMessage "Create your own drawing, or load an example to get started" "black"
    | _ ->
        match (TurtleParser.generate input) with
        // Error messages from crude parser are currently useless
        | TurtleParser.Parser.ParseResult.Error msg -> drawMessage "Oops! Not a turtle" "orange"
        | TurtleParser.Parser.ParseResult.Success d -> fun() -> d

let turtleFromModel model = (drawTurtle model.Doodle.source)()

let init doodle =
    {
        TickCount = 0
        IsEdited = false
        Doodle = doodle
    },
    [ fun d -> Fable.Core.JS.setInterval (fun _ -> d Tick) 40 |> ignore ]

let update (session:DoodleSession option) msg (model : Model)=
    Fable.Core.JS.console.log($"{msg}")
    match msg with
    | Error x -> model, Cmd.none
    | Saved t ->
        { model with Doodle = t; IsEdited = false }, Cmd.none
    | Save ->
        match session with
        | Some s ->
            model, Cmd.OfPromise.either (s.Save) (model.Doodle) Saved Error
        | None -> model, Cmd.none
    | SetName s ->
        { model with Doodle = { model.Doodle with name = s }; IsEdited = true }, Cmd.none
    | SetDescription s ->
        { model with Doodle = { model.Doodle with description = s }; IsEdited = true }, Cmd.none
    | SetSource s ->
        { model with Doodle = { model.Doodle with source = s }; IsEdited = true }, Cmd.none
    | Tick ->
        { model with TickCount = model.TickCount + 1}, Cmd.none

let nav model dispatch =
    UI.navDropdown "Examples" [
        UI.navItem "Clock" (fun _ -> SetSource Examples.clockSource |> dispatch)
        UI.navItem "Squares" (fun _ -> SetSource Examples.squareSpiralsSource |> dispatch)
        UI.navItem "Circles" (fun _ -> SetSource Examples.circleSpiralsSource |> dispatch)
    ]

let style = [
    rule ".turtle-details" [
        Css.gap (rem 0.5)
        Css.flexShrink 1
        Css.flexGrow 1
        Css.maxWidth (vh 90)
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

let turtleView turtle =
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
        ] turtle
    ]

let _view readonly model dispatch =
    UI.flexRow [

        Attr.style [
            Css.justifyContentSpaceBetween
            Css.gap (rem 4)
            Css.custom("align-items", "start")
        ]

        UI.flexColumn [
            Attr.className "turtle-details"

            model |> Store.map turtleFromModel |> turtleView

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
                        text "Save"
                        Ev.onClick (fun _ -> dispatch Save)
                    ]
                ]
        ]

        Html.textarea [
            Attr.className  "turtle-editor"
            Bind.attr("value", model .> (fun m -> m.Doodle.source), dispatch << SetSource)
        ]
    ] |> withStyle style

let view session turtle =
    let model, dispatch = turtle |> Store.makeElmish init (update session) ignore
    _view (session.IsNone) model dispatch
