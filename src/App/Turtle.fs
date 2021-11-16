module Turtle

open Sutil
open Server
open UI
open Fable.DrawingCanvas
open Fable.DrawingCanvas.Builder
open type Feliz.length
open Sutil.DOM
open Types

type Model = {
    Source : string
    //Color : string
    TickCount : int
}

type Message =
    | SetSource of string
    | Tick

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

let turtleFromModel model = (drawTurtle model.Source)()

let init() =
    { Source = ""; TickCount = 0 },
    [ fun d -> Fable.Core.JS.setInterval (fun _ -> d Tick) 40 |> ignore ]

let update msg model =
    match msg with
    | SetSource s -> { model with Source = s }, Cmd.none
    | Tick ->
        { model with
            TickCount = model.TickCount + 1
            // Color =
            //       model.Color
            //       |> ColorShift.hexToHsv
            //       |> (ColorShift.rotateHue -0.005)
            //       |> ColorShift.hsvToHex
            }, Cmd.none

let nav model dispatch =
    UI.navDropdown "Examples" [
        UI.navItem "Clock" (fun _ -> SetSource Examples.clockSource |> dispatch)
        UI.navItem "Squares" (fun _ -> SetSource Examples.squareSpiralsSource |> dispatch)
        UI.navItem "Circles" (fun _ -> SetSource Examples.circleSpiralsSource |> dispatch)
    ]

let turtleView turtle =
    Html.div [
        Attr.style [
            Css.flexGrow 1
            Css.width (px 500)
            Css.custom ("aspect-ratio", "1 / 1")
        ]
        //Bind.el( model, fun (m : Model) ->)
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

let view model dispatch =
    UI.flexRow [
        Attr.style [
            Css.justifyContentSpaceBetween
            Css.gap (rem 4)
            Css.custom("align-items", "start")
        ]

        Html.textarea [
            Attr.style [
                Css.flexGrow 1
                Css.width (px 500)
                Css.height (px 500)
                Css.backgroundColor "#202020"
                Css.color "beige"
                Css.padding (rem 1)
                Css.fontFamily "Consolas, monospace"
            ]
            Bind.attr("value", model .> (fun m -> m.Source), dispatch << SetSource)
        ]

        model |> Store.map turtleFromModel |> turtleView
    ]

let ui (session : Session) =
    let model, dispatch = () |> Store.makeElmish init update ignore
    {
        Main = view model dispatch
        Nav = nav model dispatch
    }