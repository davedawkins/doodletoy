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
# Editing and Saving

As you type into the editor, your program will be compiled and run. This means frequently you'll error messages
until you have finished.

If you are signed in you can either:

|-------------------+------------|
| `Save Changes`    | Update your doodle permanently. Feel free to change its name and give it a description
| `Discard Changes` | Throw away your edits, and load the last saved version
| `Save as Copy`    | Instead of updating the original doodle, create a new one and save it there instead. Subsequent edits will save there |

Your edit session is preserved if you navigate away, and you can come back to it later by clicking `New`, where you'll be asked if you would like to resume your existing session or start a new one.

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

Declare and assign variables using `let X := VALUE`

Use variables to control commands like `forward`, `turn`, `repeat` etc.

Give your variables any name like you like. They must start with a letter though, and don't use `t` - that's reserved for time!

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
# Control Flow

| Syntax     | Description |
|------------+-------------|
| `repeat n { commands }` | Execute `commands` `n:number` times |
| `if z { commands }` | Execute `commands` if `z:number` is non-zero |
| `if z { commands-t } else { commands-f` | Execute `commands-t` if `z:number` is non-zero, otherwise execute `commands-f` |


# Drawing Commands

| Command        | Description |
|----------------+-------------|
| `clear col`   | Set background to `col:string`. Eg clear "red", clear "#aabb11" |
| `penColor col`     | Set pen colour to `col:string` |
| `penHue hue`     | Set pen hue to `hue:number`. That is, `hsv( hue, 0, 0 )` |
| `rotateHue dh`  | Increase pen hue by `dh:number`, wrapping value to maintain range [0.0,1.0] |
| `increaseAlpha da` | Increase pen alpha by `da:number` |
| `penWidth w` | Set pen width to `n:number` |
| `increaseWidth dw` | Increase pen width by `dw:number` |
| `forward d` | Move pen in direction that it faces by `d:number` units. A line will be drawn only if pen is down |
| `turn deg` | Rotate pen by `deg:number` degrees. Positive is clockwise, negative is anticlockwise |
| `penUp` | Lift the pen up; the pen can be moved without drawing a line |
| `penDown` | Drop the pen down; moving the pen will draw a line with the current pen colour and width |
| `push` | Save the current pen state (position, up/down, colour, width) |
| `pop` | Restore the last saved pen state. Useful to instantly reset to a previous position |


# Built-in Variables

Doodle language has the following built-in variables:

| Name       | Description |
|------------+-------------|
| `t`          | Floating point time in seconds |
| `mx`         | Mouse X position in range [ -500, 500 ] |
| `my`         | Mouse Y position in range [ -500, 500 ] |

# Built-in Arithmetic Operators


| Name       | Description |
|------------+-------------|
| `a + b` |   |
| `a - b` |   |
| `a * b` |   |
| `a / b` |   |
| `a % b` |   |

# Built-in Comparison Operators


| Name       | Description |
|------------+-------------|
| `a = b`  |   |
| `a <> b` |   |
| `a < b`  |   |
| `a <= b` |   |
| `a > b`  |   |
| `a >= b` |   |

# Built-in Functions

Doodle language has the following built-in functions:

| Name         | Description |
|--------------+-------------|
| `truncate x` | Calculate integral part of floating point number |
| `frac x`     | Calculate decimal part of floating point number   |
| `sin x`      | Sine   |
| `cos x`      | Cosine   |
| `tan x`      | Tangent   |
| `asin x`     | Inverse sine   |
| `acos x`     | Inverse cosine   |
| `atan x`     | Inverse tangent   |
| `abs x`      | Absolute value   |
| `sign x`     | A number that indicates the sign of the number (-1, 0, 1)   |
| `round x`    | Round to nearest integer   |
| `sqrt x`     | Square root   |

# Defining functions

Doodle language currently has a very simple function mechanism.

- Every function takes a single argument
- Create functions with multiple arguments by nesting their definitions
- Create re-usable functions by assigning them to variable.

For example:

```
let add2 := fun n -> n + 2
```

This creates the function `fun n -> n + 2`. Its argument is named `n` and its value is `n + 2`.

The function is assigned to variable `add2`.

We can now do this:

```
forward add2 100
```

which will move the pen forward by 102 units.

Here's another example:

```
let forwardW := fun d -> fun w -> {
    penWidth w
    forward d
}
```

Here we have defined two functions, one inside the other, and assigned the outer one to `forwardW`.

We can think of this as being a way to pass two arguments to single function like this:

```
forwardW 100 10
forwardW 100 20
```

This will draw two lines of length 100 with lengths 10 and 20 respectively.

# Troubleshooting

The Doodle language is extremely basic and has challenging error reporting.

If your doodle is giving an error, check these things

- No semicolons are needed

- Commands are terminated by a `newline` character; you must therefore keep a single command on one line

- `if/then/else` and `repeat` may have newlines before the `else` and `{` keywords.

- Brackets aren't in general needed, but add them to make the order of evaluation more explicit if you're unsure
