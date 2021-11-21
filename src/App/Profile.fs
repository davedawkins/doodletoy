module Profile

open Sutil
open Types
open Server
open Sutil.Styling
open AppwriteSdk

type Model = {
    Doodles : Schema.Doodle list
}

type Message =
    | GetDoodles
    | GotDoodles of Schema.Doodle[]

let init() =
    {
        Doodles = []
    }, Cmd.ofMsg GetDoodles

let update (session : DoodleSession) msg model =
    Fable.Core.JS.console.log($"{msg}")
    match msg with
    | GetDoodles ->
        model,
        Cmd.OfPromise.result (session.UserDoodles() |> Promise.map (fun r -> r.documents |> GotDoodles))
    | GotDoodles doodles ->
        Fable.Core.JS.console.dir(doodles)
        { model with Doodles = doodles |> List.ofArray }, Cmd.none

let style = [
    rule ".profile-turtle" [
        Css.cursorPointer
    ]
    rule ".profile-turtle:hover" [
        Css.fontWeightBold
    ]
]

let viewRecord dispatch (t : Schema.Doodle ) =
    UI.UI.flexRow [
        Attr.className "profile-turtle"
        Html.span t.name
        Ev.onClick (fun _ -> EditTurtle t |> dispatch )
    ]

let view (session : DoodleSession) dispatchExternal =
    let model , dispatch = () |> Store.makeElmish init (update session) ignore

    Html.div [
        text (session.User.name)
        Bind.each( model |> Store.map (fun m -> m.Doodles), viewRecord dispatchExternal, (fun r -> r._id) )
    ] |> withStyle style