module Fable.DrawingCanvas

open System
open Fable.Core
open Sutil
open Sutil.Core
open Sutil.CoreElements
open Sutil.DomHelpers
open Browser.Types
open Fable.Core.JS

type DrawStyle =
    | Color of string
    | Gradient of CanvasGradient
    | Pattern of CanvasPattern

let LineCapButt = "butt"
let LineCapRound = "round"
let LineCapSquare = "square"

let LineJoinBevel = "bevel"
let LineJoinRound = "round"
let LineJoinMiter = "miter"


//
// Commands that match the Canvas2D almost one to one. There are a few helper aliases
//
type CanvasCommand =
    | Resize of w:float * h:float

    | RotateFillHue of x:float
    | RotateStrokeHue of x:float
    | IncreaseLineWidth of x:float
    | IncreaseStrokeRed of x:float
    | IncreaseStrokeGreen of x:float
    | IncreaseStrokeBlue of x:float
    | IncreaseGlobalAlpha of x:float

    | Save
    | Restore
    | BeginPath
    | Fill
    | Stroke
    | TextAlign of string
    | TextBaseline of string
    | LineWidth of float
    | LineCap of string
    | LineDashOffset of float
    | LineJoin of string
    | ShadowBlur of float
    | ShadowColor of string
    | ShadowOffsetX of float
    | ShadowOffsetY of float
    | MiterLimit of float
    | SetLineDash of float array
    | FillColor of string
    | StrokeColor of string
    | GlobalAlpha of float
    | FillStyle of DrawStyle
    | StrokeStyle of DrawStyle
    | MoveTo of x:float * y:float
    | LineTo of x:float * y:float
    | ArcTo of x1:float * y1:float * x2:float * y2:float * radius:float
    | Arc of x:float * y:float * radius:float * startAngle:float * endAngle:float * anticlockwise : bool
    | Rotate of angle:float
    | Translate of x:float * y:float
    | Transform of m11:float * m12:float * m21:float * m22:float * dx:float * dy:float
    | Scale of x:float * y:float
    | Font of string
    | Rect of x:float * y:float * w:float * h:float
    | ClearRect of x:float * y:float * w:float * h:float
    | FillRect of x:float * y:float * w:float * h:float
    | StrokeRect of x:float * y:float * w:float * h:float
    | FillText of text:string * x:float * y:float * maxw:float option
    | StrokeText of text:string * x:float * y:float * maxw:float option
    | Func of (CanvasRenderingContext2D -> unit)


//
// General draw command. These will need translating (compiling, kind of) into Canvas commands
//
type DrawCommand =
    | Canvas of CanvasCommand
    // | Turtle of TurtleCommand
    | Sub of (DrawCommand list)

module ColorShift =
    let rotate (x:float) =
        match x with
        | x when x > 1. -> x - 1.
        | x when x < 0. -> x + 1.
        | _ -> x

    let bound (x:float) =
        match x with
            | x when x > 1.0 -> 1.0
            | x when x < 0. -> 0.
            | _ -> x

    let bound255 (x:float) =
        match x with
            | x when x > 255.0 -> 255.0
            | x when x < 0. -> 0.
            | _ -> x

    let fromHexDigit c =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'A' && c <= 'F' then (int c - int 'A') + 10
        elif c >= 'a' && c <= 'f' then (int c - int 'a') + 10
        else invalidOp "Not a hex character"

    let fromHexByteString (ff:string) = fromHexDigit(ff.[0]) * 16 + fromHexDigit(ff.[1])

    let hexToRgb (text:string) =
        match text with
            | text when text.Length = 7 && text.[0] = '#'
                -> (
                    text.[ 1..2 ] |> fromHexByteString |> float,
                    text.[ 3..4 ] |> fromHexByteString |> float,
                    text.[ 5..6 ] |> fromHexByteString |> float
                )
            | _ -> invalidOp "Expected RGB hex format '#RRGGBB'"

    let rgbToHsv (r : float,g: float,b:float) =
        let mx = System.Math.Max(r, System.Math.Max(g, b))
        let mn = System.Math.Min(r, System.Math.Min(g, b))
        let d = mx - mn
        let s = if mx = 0. then 0. else (d / mx)
        let v = mx / 255.
        let h =
            match mx with
                | x when (x = mn) -> 0.0
                | x when (x = r) ->
                    ((g-b) + d * (if g<b then 6.0 else 0.)) / (6.0 * d)
                | x when (x = g) ->
                    ((b-r) + d * 2.0) / (6.0 * d)
                | x when (x = b) ->
                    ((r-g) + d * 4.0) / (6.0 * d)
                | _ -> invalidOp "not possible"
        (h,s,v)

    let rotateHue x (h,s,v) =
        ((h+x) |> rotate, s, v)

    let increaseRed x (r,g,b) =
        ((r + x*255.0 |> bound255), g, b)

    let increaseGreen x (r,g,b) =
        (r, (g + x*255.0 |> bound255), b)

    let increaseBlue x (r,g,b) =
        (r, g, (b + x*255.0 |> bound255))

    let rgbToHex (r,g,b) = sprintf "#%02X%02X%02X" (int r) (int g) (int b)

    let hsvToRgb (h:float,s:float,v:float) =
        let i = Math.floor (h * 6.0)
        let f = h * 6. - i
        let p = v * (1. - s)
        let q = v * (1. - f * s)
        let t = v * (1. - (1. - f) * s)
        let (r,g,b) =
            match ((int i) % 6) with
                | 0 -> (v, t, p)
                | 1 -> (q, v, p)
                | 2 -> (p, v, t)
                | 3 -> (p, q, v)
                | 4 -> (t, p, v)
                | 5 -> (v, p, q)
                | _ -> invalidOp "Stop compiler warning for incomplete matches"
        (
            Math.round(r * 255.0),
            Math.round(g * 255.0),
            Math.round(b * 255.0)
        )

    // hex is #RRGGBB
    let hsvToHex = hsvToRgb >> rgbToHex
    let hexToHsv = hexToRgb >> rgbToHsv

let rotateHueFromStyle style amount =
    let rgbHex =
        match style with
            | U3.Case1 c -> c
            | _ -> "#000000"
    U3.Case1 (rgbHex |> ColorShift.hexToHsv |> ColorShift.rotateHue amount |> ColorShift.hsvToHex)

let increaseRedFromStyle style amount =
    let rgbHex =
        match style with
            | U3.Case1 c -> c
            | _ -> "#000000"
    U3.Case1 (rgbHex |> ColorShift.hexToRgb |> ColorShift.increaseRed amount |> ColorShift.rgbToHex)

let increaseGreenFromStyle style amount =
    let rgbHex =
        match style with
            | U3.Case1 c -> c
            | _ -> "#000000"
    U3.Case1 (rgbHex |> ColorShift.hexToRgb |> ColorShift.increaseGreen amount |> ColorShift.rgbToHex)

let increaseBlueFromStyle style amount =
    let rgbHex =
        match style with
            | U3.Case1 c -> c
            | _ -> "#000000"
    U3.Case1 (rgbHex |> ColorShift.hexToRgb |> ColorShift.increaseBlue amount |> ColorShift.rgbToHex)

//
// Side-effect time for canvas commands
//
let rec runCommand (ctx:CanvasRenderingContext2D) (command : CanvasCommand) =
    match command with

    // Helper for resizing
    | Resize (w,h) -> ctx.canvas.width <- w; ctx.canvas.height <- h

    | RotateStrokeHue x ->
        ctx.strokeStyle <- rotateHueFromStyle ctx.strokeStyle x

    | RotateFillHue x ->
        ctx.fillStyle <- rotateHueFromStyle ctx.fillStyle x

    | IncreaseLineWidth x ->
        //console.log(sprintf "increaseLineWidth %f by %f" ctx.lineWidth x)
        ctx.lineWidth <- ctx.lineWidth + x

    | IncreaseGlobalAlpha x ->
        ctx.globalAlpha <- System.Math.Max(0., System.Math.Min(ctx.globalAlpha+x, 1.0))

    | IncreaseStrokeRed x ->
        ctx.strokeStyle <- increaseRedFromStyle ctx.strokeStyle x

    | IncreaseStrokeGreen x ->
        ctx.strokeStyle <- increaseGreenFromStyle ctx.strokeStyle x

    | IncreaseStrokeBlue x ->
        ctx.strokeStyle <- increaseBlueFromStyle ctx.strokeStyle x

    // Canvas2D API, with occasional helper like FillColor and StrokeColor
    | Save ->
        //console.log("save")
        ctx.save()
    | Restore ->
        //console.log("restore")
        ctx.restore()
    | BeginPath ->
        //console.log("beginPath")
        ctx.beginPath()
    | Fill ->
        //console.log("fill")
        ctx.fill()
    | Stroke ->
        //console.log("stroke")
        ctx.stroke()
    | LineCap style -> ctx.lineCap <- style
    | LineDashOffset offset -> ctx.lineDashOffset <- offset
    | LineJoin style -> ctx.lineJoin <- style
    | LineWidth n ->
        //console.log(sprintf "lineWidth %f" n)
        ctx.lineWidth <- n
    | ShadowBlur amount -> ctx.shadowBlur <- amount
    | ShadowColor color -> ctx.shadowColor <- color
    | ShadowOffsetX offset -> ctx.shadowOffsetX <- offset
    | ShadowOffsetY offset -> ctx.shadowOffsetY <- offset
    | MiterLimit n -> ctx.miterLimit <- n
    | SetLineDash segments -> ctx.setLineDash(segments)
    | FillColor c -> ctx.fillStyle <- U3.Case1(c) // Helper
    | FillStyle (Color s) -> ctx.fillStyle <- U3.Case1(s)
    | FillStyle (Gradient g) -> ctx.fillStyle <- U3.Case2(g)
    | FillStyle (Pattern p) ->  ctx.fillStyle <- U3.Case3(p)
    | StrokeColor c -> ctx.strokeStyle <- U3.Case1(c) // Helper
    | GlobalAlpha a -> ctx.globalAlpha <- System.Math.Max(0., System.Math.Min(a, 1.0))
    | StrokeStyle (Color s) -> ctx.strokeStyle <- U3.Case1(s)
    | StrokeStyle (Gradient g) -> ctx.strokeStyle <- U3.Case2(g)
    | StrokeStyle (Pattern p) -> ctx.strokeStyle <- U3.Case3(p)
    | MoveTo (x,y) ->
        //console.log("moveTo")
        ctx.moveTo(x,y)
    | LineTo (x,y) ->
        //console.log("lineTo")
        ctx.lineTo(x,y)
    | ArcTo (x1,y1,x2,y2,r) -> ctx.arcTo(x1,y1,x2,y2,r)
    | Arc (x,y,r,startAngle,endAngle,acw) -> ctx.arc(x,y, r,startAngle,endAngle,acw)
    | Rotate a -> ctx.rotate(a)
    | TextAlign a -> ctx.textAlign <- a
    | TextBaseline a -> ctx.textBaseline <- a
    | Translate (x,y) -> ctx.translate(x,y)
    | Transform (m11,m12,m21,m22,dx,dy) -> ctx.transform(m11,m12,m21,m22,dx,dy)
    | Scale (x,y) -> ctx.scale(x,y)
    | Font name -> ctx.font <- name
    | Rect (x,y,w,h) -> ctx.rect(x,y,w,h)
    | ClearRect (x,y,w,h) -> ctx.clearRect(x,y,w,h)
    | FillRect (x,y,w,h) -> ctx.fillRect(x,y,w,h)
    | StrokeRect (x,y,w,h) -> ctx.strokeRect(x,y,w,h)
    | FillText (text,x,y,maxw) ->
        match maxw with
        | None -> ctx.fillText(text,x,y)
        | Some mw -> ctx.fillText(text,x,y,mw)
    | StrokeText (text,x,y,maxw) ->
        match maxw with
        | None -> ctx.strokeText(text,x,y)
        | Some mw -> ctx.strokeText(text,x,y,mw)
    | Func f -> f ctx

let translate commands =
    let rec tr commands =
        seq {
            for cmd in commands do
                match cmd with
                | Canvas c -> yield c
                | Sub cmds -> yield! (tr cmds)
        }
    tr commands

let runCommands ctx commands =
     for cmd in commands |> translate  do
         runCommand ctx cmd

type Drawing = DrawCommand list

type DrawFunction = CanvasRenderingContext2D -> unit

type Redraw =
    | Drawing of (unit -> Drawing)
    | DrawFunction of DrawFunction

let resize (ctx : CanvasRenderingContext2D) =
    ctx.canvas.width <- ctx.canvas.offsetWidth
    ctx.canvas.height <- ctx.canvas.offsetHeight

open Fable.Core.JsInterop

[<AbstractClass>]
type DOMPoint =
    [<Emit("new DOMPointReadOnly($0,$1)")>]
    static member Create( x: float, y: float ) : DOMPoint = jsNative

    [<Emit("$0.x")>]
    abstract member x : float
    [<Emit("$0.y")>]
    abstract member y : float

type DOMMatrix =
    abstract member transformPoint: (DOMPoint -> DOMPoint)
    abstract member inverse: unit -> DOMMatrix

let initContext (ctx : CanvasRenderingContext2D) =
    let w = ctx.canvas.clientWidth
    let h = ctx.canvas.clientHeight

    if ctx.canvas.width <> w || ctx.canvas.height <> h then
        ctx.canvas.width <- w
        ctx.canvas.height <- h
        ctx.translate(w/2.0,h/2.0)
        ctx.scale(w / 1000.0, h / 1000.0)
        ctx.lineJoin <- "round"

    ()

let runDrawing (ctx : CanvasRenderingContext2D) drawing =
    initContext ctx
    ctx.save()
    try
        drawing ctx
    with
    | x ->
        Fable.Core.JS.console.error(x.Message,x)
    //drawing |> (ctx |> runCommands (makeTurtle()))
    ctx.restore()

type DrawingCanvasOptions = {
    Drawing : System.IObservable<DrawFunction>
    //Redraw : DrawFunction
    OnMouseMove : ((float * float) -> unit) option
}

let DrawingCanvas (props : seq<SutilElement>) (options : DrawingCanvasOptions) =

    let mutable subscription : IDisposable = Unchecked.defaultof<_>

    let cleanup() = subscription.Dispose()

    let onMouseMove callback (e : MouseEvent) =
        let ce = e.target :?> HTMLCanvasElement
        let ctx = ce.getContext_2d ()

        initContext ctx

        let r = ce.getBoundingClientRect()

        let mx = e.x - r.left
        let my = e.y - r.top

        let matrix : DOMMatrix = ctx?getTransform()
        let pt = matrix.inverse().transformPoint( DOMPoint.Create(mx, my) )
        callback(pt.x,pt.y)

    let render (e : HTMLElement) =
        let ce = e :?> HTMLCanvasElement

        let ctx = ce.getContext_2d ()

        rafu (fun () -> subscription <- options.Drawing.Subscribe( fun d -> runDrawing ctx d))

    Html.canvas [
        unsubscribeOnUnmount [cleanup]
        host render
        yield! props
        match options.OnMouseMove with
        | None -> fragment []
        | Some f -> Ev.onMouseMove (fun e -> onMouseMove f e)
    ]

module Builder =
    // https://stackoverflow.com/questions/23122639/how-do-i-write-a-computation-expression-builder-that-accumulates-a-value-and-als

    type BuilderVariant =
        | Regular
        | SaveRestore
        | FillPath
        | StrokePath

    type DrawCommandBuilder( variant : BuilderVariant ) =
        let append xs (x : CanvasCommand) =
            xs @ [ Canvas x ]

        // With Delay/Run:
        //     Run( Delay ( fun () -> (Yield |> Resize |> Arc) ))
        // Without
        //     Yield |> Resize |> Arc

        // Defers execution of the CE until the Run().
        member _.Delay(funcToDelay) =
            let delayed = fun () -> funcToDelay()
            delayed // return the new function. This will be unwrapped with Run, where we can apply variant

        // Initialises the expression
        member _.Yield _ =
            []

        // Unwraps the function created by Delay. This allows us to apply the variant wrapping, if needed
        member _.Run(funcToRun) : (unit -> DrawCommand list)=
            fun () ->
                let result : DrawCommand list = funcToRun()
                match variant with
                    | Regular -> result
                    | SaveRestore -> (Canvas Save :: result) @ [ Canvas Restore ]
                    | FillPath -> (Canvas BeginPath :: result) @ [ Canvas Fill ]
                    | StrokePath -> (Canvas BeginPath :: result) @ [ Canvas Stroke ]

        [<CustomOperation "resize">]
        member _.Resize(state:Drawing, w, h) = append state <| Resize (w,h)

        [<CustomOperation "rotateStrokeHue">]
        member _.RotateStrokeHue(state:Drawing, x) = append state <| RotateStrokeHue x

        [<CustomOperation "rotateFillHue">]
        member _.RotateFillHue(state:Drawing, x) = append state <| RotateFillHue x

        [<CustomOperation "increaseLineWidth">]
        member _.IncreaseLineWidth(state:Drawing, x) = append state <| IncreaseLineWidth x

        [<CustomOperation "increaseStrokeRed">]
        member _.IncreaseStrokeRed(state:Drawing, x) = append state <| IncreaseStrokeRed x

        [<CustomOperation "increaseStrokeBlue">]
        member _.IncreaseStrokeBlue(state:Drawing, x) = append state <| IncreaseStrokeBlue x

        [<CustomOperation "increaseStrokeGreen">]
        member _.IncreaseStrokeGreen(state:Drawing, x) = append state <| IncreaseStrokeGreen x

        [<CustomOperation "globalAlpha">]
        member _.GlobalAlpha(state:Drawing, x) = append state <| GlobalAlpha x

        [<CustomOperation "save">]
        member _.Save(state:Drawing ) = append state Save

        [<CustomOperation "restore">]
        member _.Restore(state:Drawing ) = append state Restore

        [<CustomOperation "beginPath">]
        member _.BeginPath(state:Drawing ) = append state BeginPath

        [<CustomOperation "fill">]
        member _.Fill(state:Drawing ) = append state Fill

        [<CustomOperation "stroke">]
        member _.Stroke(state:Drawing ) = append state Stroke

        [<CustomOperation "lineWidth">]
        member _.LineWidth(state:Drawing, w ) = append state <| LineWidth w

        [<CustomOperation "lineJoin">]
        member _.LineJoin(state:Drawing, style ) = append state <| LineJoin style

        [<CustomOperation "lineCap">]
        member _.LineCap(state:Drawing, style ) = append state <| LineCap style

        [<CustomOperation "lineDashOffset">]
        member _.LineDashOffset(state:Drawing, offset ) = append state <| LineDashOffset offset

        [<CustomOperation "shadowBlur">]
        member _.ShadowBlur(state:Drawing, amount ) = append state <| ShadowBlur amount

        [<CustomOperation "shadowColor">]
        member _.ShadowColor(state:Drawing, color ) = append state <| ShadowColor color

        [<CustomOperation "shadowOffsetX">]
        member _.ShadowOffsetX(state:Drawing, offset ) = append state <| ShadowOffsetX offset

        [<CustomOperation "shadowOffsetY">]
        member _.ShadowOffsetY(state:Drawing, offset ) = append state <| ShadowOffsetY offset

        [<CustomOperation "textAlign">]
        member _.TextAlign(state:Drawing, a ) = append state <| TextAlign a

        [<CustomOperation "textBaseline">]
        member _.TextBaseline(state:Drawing, a ) = append state <| TextBaseline a

        [<CustomOperation "miterLimit">]
        member _.MiterLimit(state:Drawing, n ) = append state <| MiterLimit n

        [<CustomOperation "setLineDash">]
        member _.SetLineDash(state:Drawing, segments ) = append state <| SetLineDash segments

        [<CustomOperation "fillColor">]
        member _.FillColor(state:Drawing, (color:string) ) = append state <| FillStyle (Color color)

        [<CustomOperation "fillStyle">]
        member _.FillStyle(state:Drawing, (style:DrawStyle) ) = append state <| FillStyle style

        [<CustomOperation "strokeColor">]
        member _.StrokeColor(state:Drawing, (color:string) ) = append state <| StrokeStyle (Color color)

        [<CustomOperation "strokeStyle">]
        member _.StrokeStyle(state:Drawing, (style:DrawStyle) ) = append state <| StrokeStyle style

        [<CustomOperation "moveTo">]
        member _.MoveTo(state:Drawing, x, y ) = append state <| MoveTo (x,y)

        [<CustomOperation "lineTo">]
        member _.LineTo(state:Drawing, x, y ) = append state <| LineTo (x,y)

        [<CustomOperation "arcTo">]
        member _.ArcTo(state:Drawing, x1, y1, x2, y2, r ) = append state <| ArcTo (x1, y1, x2, y2, r)

        [<CustomOperation "arc">]
        member _.Arc(state:Drawing, x,y,r,startAngle,endAngle, acw : bool ) = append state <| Arc (x,y,r,startAngle,endAngle,acw)

        [<CustomOperation "rotate">]
        member _.Rotate(state:Drawing, a ) = append state <| Rotate a

        [<CustomOperation "transform">]
        member _.Transform(state:Drawing, m11,m12,m21,m22,dx,dy) = append state <| Transform (m11,m12,m21,m22,dx,dy)

        [<CustomOperation "translate">]
        member _.Translate(state:Drawing, x,y ) = append state <| Translate (x,y)

        [<CustomOperation "scale">]
        member _.Scale(state:Drawing, x,y ) = append state <| Scale (x,y)

        [<CustomOperation "font">]
        member _.Font(state:Drawing, name ) = append state <| Font name

        [<CustomOperation "clearRect">]
        member _.ClearRect(state:Drawing, x,y,w,h ) = append state <| ClearRect (x,y,w,h)

        [<CustomOperation "rect">]
        member _.Rect(state:Drawing, x,y,w,h ) = append state <| Rect (x,y,w,h)

        [<CustomOperation "fillRect">]
        member _.FillRect(state:Drawing, x,y,w,h ) = append state <| FillRect (x,y,w,h)

        [<CustomOperation "strokeRect">]
        member _.StrokeRect(state:Drawing, x,y,w,h ) = append state <| StrokeRect (x,y,w,h)

        [<CustomOperation "fillText">]
        member _.FillText(state:Drawing, text, x, y, ?maxw ) = append state <| FillText (text,x,y,maxw)

        [<CustomOperation "strokeText">]
        member _.StrokeText(state:Drawing, text, x, y, ?maxw ) = append state <| StrokeText (text,x,y,maxw)

        [<CustomOperation "sub">]
        member _.Sub(state:Drawing, drawing : unit -> Drawing) = state @ drawing()

        [<CustomOperation "sub">]
        member _.Sub(state:Drawing, drawing : DrawFunction) =  append state <| Func drawing

        [<CustomOperation "repeat">]
        member _.Repeat<'T>(state:Drawing, col:seq<'T>, f:('T -> (unit ->Drawing)) ) =
            let mutable result = state
            for x in col do
                let d = f x
                result <- result @ d()
            result

        [<CustomOperation "ifThen">]
        member _.IfThen(state:Drawing, cond, succ : unit -> Drawing) : Drawing =
            if cond then (state @ succ()) else state

        [<CustomOperation "ifThenElse">]
        member _.IfThenElse(state:Drawing, cond, succ : unit -> Drawing, fail : unit -> Drawing) =
            if cond then (state @ succ()) else (state @ fail())

    let drawing = DrawCommandBuilder(Regular)
    let fillpath = DrawCommandBuilder(FillPath)
    let strokepath = DrawCommandBuilder(StrokePath)
    let preserve = DrawCommandBuilder(SaveRestore)

module ListHelpers =
    let loop coll fn = coll |> List.collect fn |> Sub

    let ifThen cond (succ : Lazy<DrawCommand list>) =
        Sub (if cond then succ.Value else [])

    let ifThenElse cond (succ : Lazy<DrawCommand list>) (fail:Lazy<DrawCommand list>) =
        Sub (if cond then succ.Value else fail.Value)

    let preserve (drawing : DrawCommand list) =
        (Canvas Save :: drawing) @ [ Canvas Restore ]

    let fillpath (drawing : DrawCommand list) =
        (Canvas BeginPath :: drawing) @ [ Canvas Fill ]

    let strokepath (drawing : DrawCommand list) =
        (Canvas BeginPath :: drawing) @ [ Canvas Stroke ]
