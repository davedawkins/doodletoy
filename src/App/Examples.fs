module Examples


let templateSource = """clear "#202020"
penDown
penHue frac (t / 10)

let d := 1
repeat 8 {
       forward d * 100
       turn 90
       let d := d + 1
       rotateHue -0.002
       increaseWidth 2
       increaseAlpha -0.005
}"""

let circleSpiralsSource = """clear "#333333"

penDown
penHue frac (t / 10)

let d := 1
repeat 600 {
       forward d * 2
       turn 49.5
       let d := d + 0.5
       rotateHue -0.0004
       increaseWidth 0.04
       increaseAlpha -0.002
}"""

let squareSpiralsSource = """clear "#333333"

penDown
penHue frac (t / 10)

let d := 1
repeat 200 {
       forward d * 2
       turn 89.5
       let d := d + 2.5
       rotateHue -0.002
       increaseWidth 0.02
       increaseAlpha -0.005
}"""

let clockSource = """clear "white"

turn -90
let tsecs = truncate t
let clockSecs := tsecs % 60
let clockMin := (tsecs / 60) % 60
let clockHr := (tsecs / 3600) % 12

push
repeat 12 {
  turn 30
  push
    forward 420
    increaseWidth 8
    penDown
      forward 12
    penUp
  pop
}
pop

push
  turn clockSecs * 6
  penDown
    penColor "silver"
    forward 360
  penUp
pop

push
  turn clockMin * 6
  penDown
    increaseWidth 2
    penColor "#999999"
    forward 400
  penUp
pop

push
  turn clockHr * 30
  penDown
    increaseWidth 4
    penColor "black"
    forward 300
  penUp
pop"""
