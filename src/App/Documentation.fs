module Documentation

open Sutil
open UI

module ReferenceDocs =
    let overview() =
        Html.div [
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
let n := 4
let d := 100
repeat n {
    forward d
    turn 90
    let d := d + 10
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
let d := 250
repeat (t % 4) + 1 {
       forward d
       turn 90
}
"""
            ]]
        ]
    ]
