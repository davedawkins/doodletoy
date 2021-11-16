module Home

open Sutil

type Model = { Id : string }


type Message =
    | NoMessage

let init() =
    { Id = "" }, Cmd.none

let update msg model =
    match msg with
    | NoMessage -> model, Cmd.none

let view server =
    Html.div [
        Attr.className "hero"
        text "Featured Turtle"
        Turtle.turtleView (Turtle.drawTurtle Examples.clockSource () |> Store.make)
    ]
