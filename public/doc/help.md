# Getting Started

Each doodle is a turtle graphics program. Some basic commands:

- `forward` and `turn` to move the turtle around.
- `penDown` to draw the turtle's path
- `penUp` to move the turtle without drawing anything

The space is 1000 x 1000 units, and the turtle starts at the center. So `forward 500` will move from the center to the outside edge on the right

```penDown
forward 100
turn 90
forward 100
turn 90
forward 100
turn 90
forward 100
```

# Loops

Use `repeat N { ... }` to make the turtle repeat a list of instructions

```# Draw a square using a loop
penDown
repeat 4 {
    forward 100
    turn 90
}
```

# Variables

Declare and assign variables using 'let X = VALUE'

Use variables to control commands like 'forward', 'turn', 'repeat' etc.

Give your variables any name like you like. They must start with a letter though, and don't use 't' - that's reserved for time!

```# Draw a spiral
penDown
let n := 4
let d := 100
repeat n {
    forward d
    turn 90
    let d := d + 10
}
```

# Colour

Set the background colour with `clear`
Set the pen colour with `penColor`
Related commands: `penHue`, `rotateHue`, `increaseAlpha`

```# Draw a spiral
clear "tan"
penColor "#334433"
penDown
forward 250
```

# Animation

Doodles are redrawn every 40ms, and so if your doodle draws slightly differently each time, it will appear to animate!

The way we can make it different each time is to use the built-in variable 't'.

```# Animated drawing of a square
clear "tan"
penDown
penColor "black"
penWidth 3
let d := 250
repeat (t % 4) + 1 {
       forward d
       turn 90
}
```