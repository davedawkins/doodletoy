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

    rule ".feature-welcome" [
        Css.displayFlex
        Css.flexDirectionRow
        Css.gap (rem 5)
        //Css.flexWrapWrap
    ]

    rule ".feature-welcome h1" [
        Css.fontSize (rem 2)
        Css.marginTop (rem 2)
    ]

    rule ".home" [
        Css.displayFlex
        Css.flexDirectionColumn
        Css.gap (rem 2)
    ]

    rule ".card" [
        // Css.backgroundColor "rgb(255,255,248)"
        // Css.border(px 1, Feliz.borderStyle.solid, "#dad2e6")
        // Css.borderRadius (px 10)
        Css.displayGrid
        Css.gridTemplateColumns(2,fr 1)
        Css.padding (rem 2)
        Css.gap (rem 2)
    ]

    rule ".card p" [
        Css.marginTop (rem 1)
    ]

    rule ".card code" [
        Css.backgroundColor "#202020"
        Css.color "#dddddd"
        Css.padding (rem 1)
    ]
    rule ".card pre" [
        Css.fontFamily "Consolas,monospace"
        Css.margin 0
    ]
    rule ".card ul" [
        Css.padding 0
        Css.margin 0
        Css.listStyleTypeNone
    ]

    rule ".card li::marker" [
        Css.color "red"
    ]

    rule ".loading" [
        Css.displayFlex
        Css.alignItemsCenter
        Css.justifyContentCenter
        Css.width (px 400)
        Css.height (px 440)
    ]

    rule ".loading i" [
        Css.fontSize (rem 8)
        Css.color "#eeeeee"
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

    UI.divc "home" [
        disposeOnUnmount [ unsub ]

        UI.divc "feature-welcome" [
            Html.div [
                Html.h2 "Featured Doodle"
                Bind.el( model |> Store.map (fun m -> m.Featured), fun doodleOpt ->
                    let options = { Animation = Always; SizePx = 400 }
                    match doodleOpt with
                    | Some doodle ->
                        Browse.DoodleView.view server options doodle
                    | None ->
                        UI.divc "loading" [
                            Html.i [ Attr.className "fas fa-spinner fa-pulse" ]
                        ]
                    // let source = doodle |> Option.map (fun d -> d.source) |> Option.defaultValue (Examples.clockSource)
                    // let featured() = Turtle.drawTurtle source ()
                    // let drawingStore = Turtle.Ticker.Create 40 featured (fun _ _ -> featured()) ignore
                    // Html.div [
                    //     Attr.className "featured"
                    //     DOM.disposeOnUnmount [drawingStore]
                    //     Turtle.turtleView drawingStore
                    // ]
                )
            ]
            Html.div [
                Html.h1 "Create and share your own doodles, be inspired by other people's creations"
            ]
        ]

        UI.divc "card" [
            Html.div [
                Html.h2 "Getting Started"
                Html.p "Each doodle is a turtle program. Some basic commands:"
                Html.ul [
                    Html.li "▪ 'forward' and 'turn' to move the turtle around."
                    Html.li "▪ 'penDown' to draw the turtle's path"
                    Html.li "▪ 'penUp' to move the turtle without drawing anything"
                ]
                Html.p "The space is 1000 x 1000 units, and the turtle starts at the center. So 'forward 500' will move from the center to the outside edge on the right"
            ]
            Html.code [ Html.pre [ text
"""penDown
forward 100
turn 90
forward 100
turn 90
forward 100
turn 90
forward 100"""
            ]]
        ]

        UI.divc "card" [
            Html.div [
                Html.h2 "Loops"
                Html.p "Use 'repeat N { ... }' to make the turtle repeat a list of instructions"
            ]
            Html.code [ Html.pre [ text
"""# Draw a square using a loop
penDown
repeat 4 {
    forward 100
    turn 90
}"""
            ]]
        ]


        UI.divc "card" [
            Html.div [
                Html.h2 "Variables"
                Html.p "Declare and assign variables using 'let X = VALUE'"
                Html.p "Use variables to control commands like 'forward', 'turn', 'repeat' etc."
                Html.p "Give your variables any name like you like. They must start with a letter though, and don't use 't' - that's reserved for time!"
            ]
            Html.code [ Html.pre [ text
"""# Draw a spiral
penDown
let n = 4
let d = 100
repeat n {
    forward d
    turn 90
    let d = d + 10
}"""
            ]]
        ]


        UI.divc "card" [
            Html.div [
                Html.h2 "Colour"
                Html.p "Set the background colour with 'clear'"
                Html.p "Set the pen colour with 'penColor'"
                Html.p "Related commands: penHue, rotateHue, increaseAlpha"
            ]
            Html.code [ Html.pre [ text
"""# Draw a spiral
clear "tan"
penColor "#334433"
penDown
forward 250
"""
            ]]
        ]


        UI.divc "card" [
            Html.div [
                Html.h2 "Animation"
                Html.p "Doodles are redrawn every 40ms, and so if your doodle draws slightly differently each time, it will appear to animate!"
                Html.p "The way we can make it different each time is to use the built-in variable 't'."
            ]
            Html.code [ Html.pre [ text
"""# Animated drawing of a square
clear "tan"
penDown
penColor "black"
penWidth 3
let d = 250
repeat (t % 4) + 1 {
       forward d
       turn 90
}
"""
            ]]
        ]


        UI.divc "card social-media" [
            Html.div [
                Html.h2 "Contact"
                Html.p [
                    Html.i [ Attr.className "fa fa-github" ]
                    text " Source code available on "
                    Html.a [ Attr.href "https://github.com/DaveDawkins/Sutil";  text "github"]
                ]
                Html.p [
                    Html.i [ Attr.className "fa fa-twitter" ]
                    text " Follow me on "
                    Html.a [ Attr.href "https://twitter.com/DaveDawkins";  text "twitter"]
                ]
            ]
        ]

    ] |> withStyle style
