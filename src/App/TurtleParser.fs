module TurtleParser

open System
open Fable.DrawingCanvas


let _log (s : string) = () //Fable.Core.JS.console.log(s)

module Parser =

    type ParseResult<'T> =
        | Success of 'T
        | Error of string

    module ParseResult =
        [<CompiledName("Map")>]
        let map f self =
            match self with
            | Error s -> Error s
            | Success t -> t |> f |> Success

        [<CompiledName("MapFst")>]
        let mapFst f self =
            self |> map (fun (a,b) -> (a|>f, b))

        [<CompiledName("MapSnd")>]
        let mapSnd f self =
            self |> map (fun (a,b) -> (a, b|>f))

    type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

    let run parser input =
        let (Parser fn) = parser
        fn input

    let skipSpaceTab (input : string) = input.TrimStart( [| ' '; '\t' |] )
    let skipSpaceTabEol (input : string) = input.TrimStart()

    let skipWhite (input : string) = skipSpaceTab input // input.TrimStart()
    let atEnd (input : string) = input = ""
    let isChar c1 c2 = c1 = c2

    let map f p =
        let inner input =
            let r = run p input
            match r with
            | Error msg -> Error msg
            | Success (v,i) -> Success (v |> f, i)
        Parser inner

    let andThen first second =
        let inner input =
            match (run first input) with
            | Error msg -> Error msg
            | Success (value1, input') ->
                let result2 = run second input'
                match result2 with
                | Error msg -> Error msg
                | Success (value2, input'')
                    -> Success((value1,value2), input'')
        Parser inner

    let andThenDiscardFirst first second =
        let inner input =
            match (run first input) with
            | Error msg -> Error msg
            | Success (_, input') ->
                let result2 = run second input'
                match result2 with
                | Error msg -> Error msg
                | Success (value2, input'')
                    -> Success(value2, input'')
        Parser inner

    let andThenDiscardSecond first second =
        let inner input =
            match (run first input) with
            | Error msg -> Error msg
            | Success (value1, input') ->
                let result2 = run second input'
                match result2 with
                | Error msg -> Error msg
                | Success (_, input'')
                    -> Success(value1, input'')
        Parser inner

    let ( .>>. ) = andThen
    let ( ..>. ) = andThenDiscardFirst
    let ( .>.. ) = andThenDiscardSecond

    let orElse first second =
        let inner input =
            match (run first input) with
            | Success result -> Success result
            | Error _ -> run second input
        Parser inner

    let ( <|> ) = orElse

    let parseKeyword keyword=
        let inner (input:string) =
            let input' = skipWhite input
            if input'.StartsWith(keyword) then
                //console.log(sprintf "keyword '%s', '%s'" keyword input'.[keyword.Length..])
                Success( keyword, input'.[keyword.Length..])
            else
                Error (sprintf "Keyword not found: %s" keyword)
        Parser inner

    let parseChar predicate name =
        let inner (input:string) =
            if (input.Length = 0) || not(predicate(input.[0])) then
                Error (sprintf "Not a %s" name)
            else
                //console.log(sprintf "char '%c', '%s'" input.[0] input.[1..])
                Success(input.[0],input.[1..])
        Parser inner

    let parseDigit =
        parseChar Char.IsDigit "digit"

    let parseWhite =
        parseChar Char.IsWhiteSpace "whitespace"

    let eatWhite =
        let inner input =
            Success((), input |> skipWhite )
        Parser inner

    let eatSpaceTab =
        let inner input =
            Success((), input |> skipSpaceTab )
        Parser inner

    let eatSpaceTabEol =
        let inner input =
            Success((), input |> skipSpaceTabEol )
        Parser inner

    let parseEnd =
        let inner input =
            if (atEnd input) then
                Success(char 0,input)
             else
                Error "Not EOF"
        Parser inner

    let isNL c = c = '\n'
    let isCR c = c = '\r'

    let isNLorCR c = isNL c || isCR c

    let parseNL =
        (parseChar isNL "newline")

    let parseCR =
        (parseChar isCR "carriage return")

    let parseCRNL =
        parseKeyword "\r\n" |> map (fun _ -> '\n')

    let parseEol =
        (parseCRNL <|> parseNL)

    let parseSequence elementParser =
        let rec accum state input =
            let r = run elementParser input
            match r with
            | Error msg ->
                //console.log(sprintf "End of sequence with %s at %s" msg input)
                Success (state, input)
            | Success (cmd,input') ->
                //console.log(sprintf "Parsed sequence element %A" cmd)
                accum (state @ [cmd]) input'

        Parser (accum [])

    let parseSequenceMin (minCount : int) elementParser =
        let inner input =
            let result = run (parseSequence elementParser) input
            match result with
            | Error msg -> Error msg
            | Success (items, remainder) when (items.Length >= minCount) ->
                Success (items, remainder)
            | Success (items, _) ->
                Error (sprintf "Expected %d items, found %d" minCount (items.Length))
        Parser inner

    let optional p =
        let inner input =
            let r = run p input
            match r with
            | Error _ -> Success(None, input)
            | Success (v,remainder) -> Success(Some v, remainder)
        Parser inner

    let lookahead p =
        let inner input =
            run p input |> ParseResult.mapSnd (fun _ -> input)
        Parser inner

    let choose parsers =
        parsers |> List.reduce (<|>)

    let parseNumber =
        let inner input =
            let p =
                (optional (parseChar (isChar '-') "negate"))
                .>>. (parseSequence parseDigit)
                .>>. (optional (parseChar (isChar '.') "point" .>>. parseSequence parseDigit))
                //.>>. (parseWhite <|> parseEnd)
            let r = run p (skipWhite input)
            match r with
            | Error msg -> Error msg
            | Success (((neg,digits),dec), remainder) ->
                if digits.IsEmpty then
                    Error "Not a number"
                else
                    let dval c = (int c - int '0')
                    let m10 (n: float) (c : char) = n * 10.0 + (dval c |> float)
                    let d10 (c : char) (n: float)  = (n  + (dval c |> float)) / 10.0

                    let mant =
                        match dec with
                        | None -> 0.0
                        | Some (_,digits) -> List.foldBack d10 digits 0.0

                    let n = ((digits |> List.fold m10 0.0) + mant) * (if neg.IsSome then -1.0 else 1.0)
                    //console.log(sprintf "Number %f, '%s'" n remainder)
                    Success( n, remainder)
        Parser inner

    let parseString =
        let inner input =
            let q c = isChar c '"'
            let nq c = not (q c)

            let p = (parseChar q "quote") ..>. (parseSequence (parseChar nq "string character")) .>.. (parseChar q "quote")
            let r = run p (skipWhite input)
            match r with
            | Error msg -> Error msg
            | Success (chars, remainder) ->
                Success ( String( List.toArray chars ), remainder )
        Parser inner

    let parseIdent =
        let inner input =
            let p = (parseChar Char.IsLetter "letter") .>>. (parseSequence (parseChar Char.IsLetterOrDigit "ident character"))
            let r = run p (skipWhite input)
            match r with
            | Error msg -> Error msg
            | Success ((first,chars) , remainder) ->
                Success (sprintf "%c%s" first (String( List.toArray chars )), remainder)
        Parser inner

open Parser
open Browser.Types
open Fable.Core

let parseSingle keyword turtleCommand =
    (parseKeyword keyword) |> map (fun _ -> turtleCommand)

let parseComment =
    let inner input =
        let p = (parseChar ((=)'#') "comment")
                    .>>. (parseSequence (parseChar (not<<isNLorCR) "comment character"))
                    .>>. (parseSequence (parseChar isNLorCR "eol"))
        let r = run p input
        match r with
        | Success (((hash,comment),eol),remainder) -> Success ((String(List.toArray comment)), remainder)
        | Error msg -> Error msg

    Parser inner

let discard p = p |> map ignore

let eatComments =
    let parseWhite min = (parseSequenceMin min (parseChar Char.IsWhiteSpace "white")) |> discard
    parseWhite 1 <|> (parseSequence (parseWhite 0 ..>. (parseComment |> discard)) |> discard)

let parseSemi = (parseChar ((=) ';') "semicolon")

type TurtleState = {
    mutable IsPenDown : bool
    mutable LineCount : int
}

let makeTurtle() =
    { IsPenDown = false; LineCount = 0 }


type BinOp =
    | Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge
    | Add
    | Mul
    | Subtr
    | Div
    | Mod

type Expr =
    | Lambda of string * Expr
    | Num of float
    | Str of string
    | Id of string * Expr list
    | Bin of (BinOp*Expr*Expr)
    | Cmd of TCommand

and TCommand =
    | TCall of (string * Expr list)
    | TBlock of TCommand list
    | TRepeat of (Expr*TCommand list)
    | TIf of (Expr * TCommand list * TCommand list option)
    | TLet of (string*Expr)

type Val =
    //| Draw of DrawCommand
    | Func of (Context -> Val -> Val)
    | Proc of (Context -> unit)
    | Float of float
    | String of string
    | Unit
and Context = {
    Id : int
    mutable Vars : Map<string,Val>
    Canvas : CanvasRenderingContext2D
    State : TurtleState
}

let expectFunc (tval : Val) =
    tval |> function Func f -> f|_ -> failwith $"Not a function: {tval}"

let expectFloat (tval : Val) =
    tval |> function Float f -> f|_ -> failwith $"Not a float: {tval}"

let expectUnit (tval : Val) =
    tval |> function Unit -> ()|_ -> failwith $"Not a unit: {tval}"

let expectString (tval : Val) =
    tval |> function String s -> s|_ -> failwith $"Not a string: {tval}"

let rec optionalDrawCommandImmediate (context : Context) (tval : Val) : unit =
    tval
    |> function
    | Proc p -> p context
    //| Draw cmd -> runDrawCommand context cmd
    | Func f -> failwith $"Not enough arguments"
    | Unit -> ()
    | _ -> failwith "Function has a result but is unused"

let parseNum = eatSpaceTab ..>. parseNumber |> map Num
let parseStr = eatSpaceTab ..>. parseString |> map Str
let parseIdRef = parseIdent |> map (fun name -> Id(name,[]))

// Turn "+" -> BinOp.Add
let parseOp (token:string,op:BinOp) =
    let inner input =
        input |> run (parseKeyword token) |> ParseResult.mapFst (fun k -> op)
    Parser inner

let parseOps ops =
    ops |> List.map parseOp |> choose

let parseMulOp =
    [
        ("*",Mul)
        ("/",Div)
        ("%",Mod)
    ] |> parseOps

let parseAddOp =
    [
        ("+",Add)
        ("-",Subtr)
    ] |> parseOps

let parseRelOp =
    [
        ("=",Eq)
        ("<>",Ne)
        ("<",Lt)
        ("<=",Le)
        (">",Gt)
        (">=",Ge)
    ] |> parseOps

let parseBlockStart = parseChar (isChar '{') "left curly"
let parseBlockEnd = parseChar (isChar '}') "right curly"

let parseDelim = eatSpaceTab ..>. (discard parseEnd <|> discard parseEol <|> discard parseComment <|> (discard (lookahead parseBlockEnd)))

let buildBinTree =
    ParseResult.mapFst <| // Map result, let remainder go through unchanged
        fun (f,optTail) ->
            match optTail with
                | None -> f // Was just factor like 1.0 or x
                | Some tail -> tail |> (List.fold (fun a (op,b) -> Bin(op,a,b)) f)

let rec makeLambda (ids,expr) =
    match ids with
    | [ id ] -> Lambda (id, expr)
    | id::xs -> Lambda (id, makeLambda(xs,expr))
    | [] -> failwith "No argument supplied"

let rec parseFactor =
    (parseLambda <|> parseFun <|> parseNum <|> parseStr <|> parseIdRef <|> parseSubExpr)

and parseFun =
    (parseIdent .>>. parseSequence parseExpr) |> map Id

and parseLambdaBody =
    (parseBlock |> map (TBlock >> Cmd)) <|> (parseCommand |> map Cmd)

and parseLambda =
    parseKeyword "fun"
        ..>. (parseSequenceMin 1 parseIdent)
        .>.. (parseKeyword "->" .>>. eatSpaceTabEol)
        .>>. (parseLambdaBody <|> parseExpr)
        |> map makeLambda

and parseSubExpr =
    parseKeyword "(" ..>. parseExpr .>.. parseKeyword ")"

and parseMulExpr =
    let inner input =
        let p = parseFactor
                    .>>. ((parseMulOp .>>. parseFactor) |> parseSequence |> optional)
        run p input |> buildBinTree
    Parser inner

and parseAddExpr =
    let inner input =
        let p = parseMulExpr
                    .>>. ((parseAddOp .>>. parseMulExpr) |> parseSequence |> optional)
        run p input |> buildBinTree
    Parser inner

and parseRelOpExpr =
    let inner input =
        let p = parseAddExpr
                    .>>. ((parseRelOp .>>. parseAddExpr) |> parseSequence |> optional)
        run p input |> buildBinTree
    Parser inner

and parseExpr =
    eatSpaceTab ..>. parseRelOpExpr

and parseNumericCommand keyword =
    (andThen (parseKeyword keyword) parseExpr)

and parseStringCommand keyword =
    (andThen (parseKeyword keyword) parseExpr)

and parseCommand =
    let rec makeAssignment (ids,e) =
        match ids with
        | [id] -> TLet (id,e)
        | id::xs -> TLet (id, makeLambda(xs,e))
        | [] -> failwith "Missing assignment name"

    [
        ((parseNumericCommand "if") .>>. parseBlock .>>. (optional (eatSpaceTabEol ..>. parseKeyword "else" ..>. parseBlock)))
            |> map (fun (((_,n),cmd),elseCmd) -> TIf (n,cmd,elseCmd) )
        ((parseNumericCommand "repeat") .>>. parseBlock)
            |> map (fun ((_,n),cmd) -> TRepeat (n,cmd) )
        parseIdent .>>. (parseSequence parseExpr) .>.. parseDelim |> map TCall
        (parseKeyword "let" ..>. parseIdent .>.. parseKeyword ":=" .>>. parseExpr)
            |> map TLet
        (parseKeyword "let" ..>. (parseSequenceMin 2 parseIdent) .>.. parseKeyword ":=" .>>. ((parseBlock |> map (TBlock >> Cmd)) <|> parseExpr))
            |> map makeAssignment

    ] |> choose

and parseCommands =
    let rec accum (state : TCommand list) input =
        let stripped = skipSpaceTabEol input
        if atEnd stripped then
            Success(state,stripped)
        else
            let r = run (eatComments ..>. parseCommand) stripped
            match r with
            | Error msg ->
                if state.IsEmpty then
                    Error msg
                else
                    Success(state,stripped)
            | Success (cmd,input') ->
                accum (state @ [cmd]) input'

    Parser (accum [])

and parseBlock =
    let inner input =
        let p =
            eatComments
            ..>. parseBlockStart
            .>>. parseCommands
            .>.. eatComments
            .>>. parseBlockEnd
        let r = run p (skipWhite input)
        match r with
        | Error msg -> Error msg
        | Success (((lc, block), rc), remainder) ->
            Success (block, remainder)
    Parser inner

//  factor: id | number | string | '(' expr ')'
//  unop:
//  mul:    factor  [ ('*'|'/'|'%') factor ]
//  add:    mul [ ('+'|'-')  mul ]
//  expr:   add

let cloneVars (map : Map<string,Val>) =
    map |> Map.toArray |> Map.ofArray

let cloneContext (context) =
    context
    //{ context with Vars = cloneVars context.Vars }

let parse input =
    let r = run parseCommands input
    match r with
    | Error msg ->
        Error msg
    | Success (cmds,remainder) ->
        if remainder |> skipWhite |> atEnd then
            Success cmds
        else
            Error (sprintf "Syntax error at: %s" remainder)

let rec eval context (expr : Expr) : Val =
    //Fable.Core.JS.console.log($"[{context.Id}]: eval")

    let valOf e =
        eval context e |> expectFloat

    let ofBool b = if b then 1.0 else 0.0
    match expr with
    | Lambda (argName,expr) ->
        Func (fun fcontext argVal ->
            let localC = { fcontext with Id = fcontext.Id + 1; Vars = context.Vars.Add(argName,argVal) }
            //let keys = localC.Vars |> Map.toSeq |> Seq.map fst |> String.concat ", "
            eval localC expr
        )
    | Bin (Add,a,b) -> (valOf a) + (valOf b) |> Float
    | Bin (Subtr,a,b) -> (valOf a) - (valOf b) |> Float
    | Bin (Mul,a,b) -> (valOf a) * (valOf b) |> Float
    | Bin (Div,a,b) -> (valOf a) / (valOf b) |> Float
    | Bin (Mod,a,b) -> (valOf a) % (valOf b) |> Float
    | Bin (Eq,a,b) -> (valOf a) = (valOf b) |> ofBool |> Float
    | Bin (Ne,a,b) -> (valOf a) <> (valOf b) |> ofBool |> Float
    | Bin (Lt,a,b) -> (valOf a) < (valOf b) |> ofBool |> Float
    | Bin (Le,a,b) -> (valOf a) <= (valOf b) |> ofBool |> Float
    | Bin (Gt,a,b) -> (valOf a) > (valOf b) |> ofBool |> Float
    | Bin (Ge,a,b) -> (valOf a) >= (valOf b) |> ofBool |> Float
    | Num n -> n |> Float
    | Str s -> s |> String
    | Id (id, args) ->
        try
            evalFunc context id args
        with
        | x ->
            Fable.Core.JS.console.error($"{id}: {x.Message}")
            0.0 |> Float
    | Cmd c ->
        evalCmdImmediate context c ; Unit

and evalLambda context maybeLambda (args : Expr list)  =
    //Fable.Core.JS.console.log($"callf {maybeLambda} {args}")
    match maybeLambda, args with
    //| Func f, [] ->
    //    f Unit
    | Func f, [x] ->
        (f context) (eval context x)
    | Func f, x :: xs ->
        let partialResult = (f context) (eval context x)
        evalLambda context partialResult xs
    | immediate, [] ->
        immediate
    | f, _ ->
        failwith $"Not a function: {f}"

and evalFunc (context : Context) name args =
    if (not (context.Vars.ContainsKey name)) then
        failwith (sprintf "No function called '%s'" name)
    let f = context.Vars.[name]
    evalLambda context f args

and evalCmdsImmediate (context : Context) cmds =
    cmds |> List.iter (evalCmdImmediate context)

and evalCmdImmediate (context : Context) cmd : unit=
    match cmd with
    | TBlock block -> evalCmdsImmediate (cloneContext context) block
    | TIf (e,block,elseBlockOpt) ->
        let n = (eval context e) |> expectFloat
        if n <> 0.0 then
            evalCmdsImmediate (cloneContext context) block
        else
            match elseBlockOpt with
            | None -> ()
            | Some elseBlock -> evalCmdsImmediate (cloneContext context) elseBlock
    | TRepeat (e,block) ->
        let n = (eval context e) |> expectFloat
        [1..(int n)] |> List.iter (fun _ -> evalCmdsImmediate context block)
    | TLet (id,e) ->
        context.Vars <- context.Vars.Add(id,eval context e)
    | TCall (name,args) ->
        _log($"TCall {name} {args}")
        evalFunc (cloneContext context) name args
            |> optionalDrawCommandImmediate context// Build a Drawing from a parse tree, evaluating Exprs

let evalProgramWithVarsImmediate (vars : Map<string,Val>) canvas program =
    let mutable context = { Id = 1; Vars = vars; Canvas = canvas; State = makeTurtle() }
    evalCmdsImmediate context program
    if (context.State.LineCount > 0) then
        canvas.stroke()

let logVal (a : Val) = Fable.Core.JS.console.log($"{a}")

type BuiltIn =
    static member Of ( f : float -> float ) =
        Func (fun ctx -> (Float<<f<<expectFloat))

    //static member Of ( f : float -> float -> float ) =
    //    Func (fun ctx val0 -> val0 |> expectFunc |> fun _ val1 -> val1 |> expectFloat |> (f val0 val1) |> Float)

    static member Of ( f : unit -> unit ) =
        Func (fun c v -> v |> expectUnit |> f; Unit)

    static member Of ( f : string -> unit ) =
        Func (fun c v -> v |> expectString |> f; Unit)

    static member Of ( f : float -> unit ) =
        Func (fun c v -> v |> expectFloat |> f; Unit)

    static member Of ( f : Val -> unit ) =
        Func (fun c v -> v |> f; Unit)

    static member Of ( f : Context -> unit ) =
        Proc f

    static member Of ( f : Context -> float -> unit ) =
        Func (fun c v -> v |> expectFloat |> (f c); Unit)

    static member Of ( f : Context -> string -> unit ) =
        Func (fun c v -> v |> expectString |> (f c); Unit)

let normalize (n : float) (min : float)  (max : float) =
    (n - min) / (max - min)

module TurtleCmd =
    let stroke context =
        let turtle = context.State
        if (turtle.LineCount > 0) then
            context.Canvas.stroke()
            turtle.LineCount <- 0

    let turn context a =
        context.Canvas.rotate( a * Math.PI / 180.0 )

    let forward context n =
        let turtle = context.State
        let ctx = context.Canvas

        if (turtle.LineCount = 0 && turtle.IsPenDown) then
            ctx.beginPath()
            ctx.moveTo(0.0, 0.0)

        if (turtle.IsPenDown) then ctx.lineTo(n,0.0) else ctx.moveTo(n,0.0)
        ctx.translate(n,0.0)

        if (turtle.IsPenDown) then
            turtle.LineCount <- turtle.LineCount + 1

    let push context =
        context.Canvas.save()

    let pop context =
        stroke context
        context.Canvas.restore()

    let penUp context =
        context.State.IsPenDown <- false

    let penDown context =
        context.State.IsPenDown <- true

    let penColor context (c : string) =
        stroke context
        context.Canvas.strokeStyle <- U3.Case1 c

    let rotateHue context x =
        stroke context
        context.Canvas.strokeStyle <- rotateHueFromStyle context.Canvas.strokeStyle x

    let increaseWidth context x =
        stroke context
        context.Canvas.lineWidth <- context.Canvas.lineWidth + x

    let increaseAlpha context x =
        stroke context
        context.Canvas.globalAlpha <- System.Math.Max(0., System.Math.Min(context.Canvas.globalAlpha+x, 1.0))

    let increaseStrokeRed context x =
        context.Canvas.strokeStyle <- increaseRedFromStyle context.Canvas.strokeStyle x

    let increaseStrokeGreen context x =
        context.Canvas.strokeStyle <- increaseGreenFromStyle context.Canvas.strokeStyle x

    let increaseStrokeBlue context x =
        context.Canvas.strokeStyle <- increaseBlueFromStyle context.Canvas.strokeStyle x

    let penWidth context w =
        if (context.Canvas.lineWidth <> w) then
            stroke context
            context.Canvas.lineWidth <- w

    let clear context (fillColor:string) =
        context.Canvas.fillStyle <- U3.Case1 fillColor
        context.Canvas.fillRect (-500., -500., 1000., 1000.)

    let hue context (n : float) =
        context.Canvas.strokeStyle <-
            ColorShift.hsvToHex (ColorShift.bound n,1.0,1.0) |> U3.Case1

let initBuiltIns (vars : Map<string,Val>) =
    let vars' : Map<string,Val> =
        vars
            .Add( "t", (double(DateTime.Now.Ticks) / double(10_000_000)) |> Float)
            .Add( "pi", (Math.PI) |> Float)

            .Add( "truncate", BuiltIn.Of(fun (t:float) -> Math.Truncate t))
            .Add( "frac", BuiltIn.Of(fun (t:float) -> t - Math.Truncate t))
            .Add( "deg", BuiltIn.Of(fun (r:float) -> 180.0 * r / Math.PI))
            .Add( "rad", BuiltIn.Of(fun (r:float) -> Math.PI * r / 180.0))
            .Add( "sin", BuiltIn.Of(Math.Sin))
            .Add( "cos", BuiltIn.Of(Math.Cos))
            .Add( "tan", BuiltIn.Of(Math.Tan))
            .Add( "asin", BuiltIn.Of(Math.Asin))
            .Add( "acos", BuiltIn.Of(Math.Acos))
            .Add( "atan", BuiltIn.Of(Math.Atan))
            .Add( "abs", BuiltIn.Of(fun (n:float) -> float(Math.Abs(n))))
            .Add( "sign", BuiltIn.Of(fun (n:float) -> float(Math.Sign(n))))
            .Add( "round", BuiltIn.Of(fun (n:float) -> Math.Round(n)))
            .Add( "sqrt", BuiltIn.Of(Math.Sqrt))

            .Add( "forward", BuiltIn.Of(TurtleCmd.forward))
            .Add( "turn", BuiltIn.Of(TurtleCmd.turn))
            .Add( "penHue", BuiltIn.Of(TurtleCmd.hue))
            .Add( "penColor", BuiltIn.Of(TurtleCmd.penColor))
            .Add( "penWidth", BuiltIn.Of(TurtleCmd.penWidth))
            .Add( "clear", BuiltIn.Of(TurtleCmd.clear))
            .Add( "rotateHue", BuiltIn.Of(TurtleCmd.rotateHue))
            .Add( "increaseWidth", BuiltIn.Of(TurtleCmd.increaseWidth))
            .Add( "increaseAlpha", BuiltIn.Of(TurtleCmd.increaseAlpha))

            .Add( "penDown", BuiltIn.Of(TurtleCmd.penDown))
            .Add( "penUp", BuiltIn.Of(TurtleCmd.penUp))

            .Add( "push", BuiltIn.Of(TurtleCmd.push))
            .Add( "pop", BuiltIn.Of(TurtleCmd.pop))

            .Add( "log", BuiltIn.Of(logVal))
    vars'

let createRedraw (vars : Map<string,Val>) program : DrawFunction = fun ctx ->
    let vars' = initBuiltIns vars
    evalProgramWithVarsImmediate vars' ctx program

let generateRedraw input vars =
    parse input
        |> ParseResult.map (createRedraw vars)
