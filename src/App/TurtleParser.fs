module TurtleParser

open System
open Fable.DrawingCanvas
open Browser.Dom
open System.Collections.Generic
open Fable.DrawingCanvas.Builder


let _log s = Fable.Core.JS.console.log(s)

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

    let parseEnd =
        let inner input =
            if (atEnd input) then
                Success(char 0,input)
             else
                Error "Not EOF"
        Parser inner

    let parseNL =
        (parseChar (fun c -> c = '\n') "newline")

    let parseCR =
        (parseChar (fun c -> c = '\r') "carriage return")

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

let parseSingle keyword turtleCommand =
    (parseKeyword keyword) |> map (fun _ -> turtleCommand)

let parseSemi = (parseChar ((=) ';') "semicolon")
let parseDelimx = eatSpaceTab ..>. (parseSemi)
let parseDelim = eatSpaceTab ..>. (parseEnd <|> parseEol)

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

type TCommand =
    | TCall of (string * Expr list)
    | TBlock of TCommand list
    | TRepeat of (Expr*TCommand list)
    | TLet of (string*Expr)

type Val =
    | Draw of DrawCommand
    | Func of (Val -> Val)
    | Float of float
    | String of string
    | Unit

let expectFloat (tval : Val) =
    tval |> function Float f -> f|_ -> failwith "Not a float"

let expectUnit (tval : Val) =
    tval |> function Unit -> ()|_ -> failwith "Not a float"

let expectString (tval : Val) =
    tval |> function String s -> s|_ -> failwith "Not a string"

let optionalDrawCommand (tval : Val) =
    tval |> function Draw cmd -> cmd|_ -> Sub []

type Context = {
    Vars : Map<string,Val>
}

let parseNum = eatWhite ..>. parseNumber |> map Num
let parseStr = eatWhite ..>. parseString |> map Str
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

let buildBinTree =
    ParseResult.mapFst <| // Map result, let remainder go through unchanged
        fun (f,optTail) ->
            match optTail with
                | None -> f // Was just factor like 1.0 or x
                | Some tail -> tail |> (List.fold (fun a (op,b) -> Bin(op,a,b)) f)

let rec parseFactor =
    (parseLambda <|> parseFun <|> parseNum <|> parseStr <|> parseIdRef <|> parseSubExpr)

and parseFun =
    (parseIdent .>>. parseSequence parseExpr) |> map Id

and parseLambda =
    parseKeyword "fun" ..>. parseIdent .>.. parseKeyword "->" .>>. parseExpr
        |> map Lambda

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

and parseExpr =
    eatWhite ..>. parseAddExpr

let parseNumericCommand keyword =
    (andThen (parseKeyword keyword) parseExpr)

let parseStringCommand keyword =
    (andThen (parseKeyword keyword) parseExpr)

let rec parseCommand =
    [
        ((parseNumericCommand "repeat") .>>. parseBlock)
            |> map (fun ((_,n),cmd) -> TRepeat (n,cmd) )
        parseIdent .>>. (parseSequence parseExpr) .>.. parseDelim |> map TCall
        (parseKeyword "let" ..>. parseIdent .>.. parseKeyword "=" .>>. parseExpr)
            |> map (fun (id,e) -> TLet (id,e))

    ] |> choose
(*
and parseCommand1 =
    [
        (parseKeyword "clear" .>>. (optional parseString))
            |> map (fun (_,bg) ->
                let fillColor =
                    match bg with
                        | None -> "white"
                        | Some s -> s
                Sub [
                    Canvas (FillColor fillColor)
                    Canvas (FillRect (-500., -500., 1000., 1000.))
                ] |> TDrawCommand
            )
        (parseNumericCommand "forward") |> map (fun (_,e) -> TNumeric (e,Forward>>Turtle))
        (parseNumericCommand "turn") |> map (fun (_,e) -> TNumeric (e,Turn>>Turtle))
        (parseNumericCommand "penColor") |> map (fun (_,e) -> TString (e,PenColor>>Turtle))
        (parseNumericCommand "rotateHue") |> map (fun (_,e) -> TNumeric (e,RotateHue>>Turtle))
        (parseNumericCommand "increaseWidth") |> map (fun (_,e) -> TNumeric (e,IncreaseWidth>>Turtle))
        (parseNumericCommand "increaseAlpha") |> map (fun (_,e) -> TNumeric (e,IncreaseAlpha>>Turtle))

        (parseBlock |> map TBlock)

        (parseKeyword "let" ..>. parseIdent .>.. parseKeyword "=" .>>. parseExpr)
            |> map (fun (id,e) -> TLet (id,e))

        ((parseNumericCommand "repeat") .>>. parseBlock)
            |> map (fun ((_,n),cmd) -> TRepeat (n,cmd) )

        parseSingle "penUp" (PenUp|> Turtle |> TDrawCommand)
        parseSingle "penDown" (PenDown|> Turtle |> TDrawCommand)
        parseSingle "push" (Push |> Turtle |> TDrawCommand)
        parseSingle "pop" (Pop |> Turtle |> TDrawCommand)

    ] |> choose
*)
and parseCommands =
    let rec accum (state : TCommand list) input =
        let stripped = skipSpaceTabEol input
        if atEnd stripped then
            Success(state,stripped)
        else
            let r = run parseCommand stripped
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
            eatWhite
            ..>. parseChar (isChar '{') "left curly"
            .>>. parseCommands
            .>.. eatWhite
            .>>. parseChar (isChar '}') "right curly"
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
    let valOf e =
        eval context e |> expectFloat

    let ofBool b = if b then 1.0 else 0.0
    match expr with
    | Lambda (argName,expr) ->
        Func (fun argVal ->
            let localC = { Vars = context.Vars.Add(argName,argVal) }
            eval localC expr
        )
    | Bin (Add,a,b) -> (valOf a) + (valOf b) |> Float
    | Bin (Subtr,a,b) -> (valOf a) - (valOf b) |> Float
    | Bin (Mul,a,b) -> (valOf a) * (valOf b) |> Float
    | Bin (Div,a,b) -> (valOf a) / (valOf b) |> Float
    | Bin (Mod,a,b) -> (valOf a) % (valOf b) |> Float
    | Bin (Eq,a,b) -> (valOf a) = (valOf b) |> ofBool |> Float
    | Bin (Ne,a,b) -> (valOf a) <> (valOf b) |> ofBool |> Float
    | Bin (Lt,a,b) -> (valOf a) <= (valOf b) |> ofBool |> Float
    | Bin (Le,a,b) -> (valOf a) >= (valOf b) |> ofBool |> Float
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
and evalLambda context maybeLambda (args : Expr list)  =
    //Fable.Core.JS.console.log($"callf {fe} {args0}")
    match maybeLambda, args with
    //| Func f, [] ->
    //    f Unit
    | Func f, [x] ->
        f (eval context x)
    | Func f, x :: xs ->
        let partialResult = f (eval context x)
        evalLambda context partialResult xs
    | immediate, [] -> immediate
    | f, _ ->
        failwith $"Not a function: {f}"

and evalFunc (context : Context) name args =
    if (not (context.Vars.ContainsKey name)) then
        failwith (sprintf "No function found called '%s'" name)
    let f = context.Vars.[name]
    evalLambda context f args

// Build a Drawing from a parse tree, evaluating Exprs
let evalProgramWithVars (vars : Map<string,Val>) program =

    let mutable context = { Vars = vars }

    let rec evalCmds cmds =
        cmds |> List.fold (fun list cmd -> list @ [evalCmd cmd]) []
    and evalCmd cmd =
        match cmd with
        | TBlock block -> Sub (evalCmds block)
        | TRepeat (e,block) ->
            let n = (eval context e) |> expectFloat
            ListHelpers.loop [1..(int n)] (fun _ -> evalCmds block)
        | TLet (id,e) ->
            context <- { context with Vars = context.Vars.Add(id,eval context e) }
            Sub []
        | TCall (name,args) ->
            //_log($"TCall {name}")
            evalFunc context name args
                |> optionalDrawCommand

    evalCmds program

let logVal (a : Val) = _log($"{a}")

type BuiltIn =
    static member Of ( f : float -> float ) =
        Func (Float<<f<<expectFloat)

    static member Of ( f : unit -> unit ) =
        Func (fun v -> v |> expectUnit |> f; Unit)

    static member Of ( f : string -> unit ) =
        Func (fun v -> v |> expectString |> f; Unit)

    static member Of ( f : Val -> unit ) =
        Func (fun v -> v |> f; Unit)

    static member Of ( t : TurtleCommand ) =
        t |> Turtle |> Draw

    static member Of ( f : float -> TurtleCommand ) =
        Func (Draw<<Turtle<<f<<expectFloat)

    static member Of ( f : string -> TurtleCommand ) =
        Func (Draw<<Turtle<<f<<expectString)

    static member Of ( f : string -> DrawCommand ) =
        Func (Draw<<f<<expectString)

let clear (fillColor:string) =
    Sub [
        Canvas (FillColor fillColor)
        Canvas (FillRect (-500., -500., 1000., 1000.))
    ]

let hue (n : float) =
    ColorShift.hsvToHex (ColorShift.bound n,1.0,1.0) |> PenColor

let normalize (n : float) (min : float)  (max : float) =
    (n - min) / (max - min)

let normalize1000 (n : float) = normalize n 0.0 1000.0

let evalProgram program =
    let vars : Map<string,Val> =
        Map.empty
            .Add( "t", (double(DateTime.Now.Ticks) / double(10_000_000)) |> Float)

            .Add( "truncate", BuiltIn.Of(fun (t:float) -> Math.Truncate t))
            .Add( "frac", BuiltIn.Of(fun (t:float) -> t - Math.Truncate t))
            .Add( "sin", BuiltIn.Of(Math.Sin))
            .Add( "cos", BuiltIn.Of(Math.Cos))
            .Add( "round", BuiltIn.Of(fun (n:float) -> Math.Round(n)))
            .Add( "sqrt", BuiltIn.Of(Math.Sqrt))

            .Add( "forward", BuiltIn.Of(Forward))
            .Add( "turn", BuiltIn.Of(Turn))
            .Add( "penHue", BuiltIn.Of(hue))
            .Add( "penColor", BuiltIn.Of(PenColor))
            .Add( "clear", BuiltIn.Of(clear))
            .Add( "rotateHue", BuiltIn.Of(RotateHue))
            .Add( "increaseWidth", BuiltIn.Of(IncreaseWidth))
            .Add( "increaseAlpha", BuiltIn.Of(IncreaseAlpha))

            .Add( "penDown", BuiltIn.Of(PenDown))
            .Add( "penUp", BuiltIn.Of(PenUp))

            .Add( "push", BuiltIn.Of(Push))
            .Add( "pop", BuiltIn.Of(Pop))

            .Add( "log", BuiltIn.Of(logVal))
    try
        let drawing = evalProgramWithVars vars program
        //for c in (drawing |> Array.ofList) do
        //    Fable.Core.JS.console.dir($"{c}")
        drawing
    with
    | x ->
        Fable.Core.JS.console.error(x.Message)
        []

let makeLazy x = fun () -> x

let generate input =
    parse input
        |> ParseResult.map evalProgram

// Parse a user drawing into a LazyDrawing
let generateWithPenColor input color =
    generate input
        |> ParseResult.map (fun d -> (PenColor color |> Turtle) :: d)

