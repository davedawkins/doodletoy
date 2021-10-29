module App

open Sutil
open type Feliz.length
open AppwriteSdk
open Server
open Sutil.DOM

type Page =
    | Login
    | Chat

type Model = {
    Page : Page
    Session : Session option
}

type Message =
    | SetPage of Page
    | SignedIn of User
    | SignedOut

let init() =
    { Page = Login; Session = None }, Cmd.none

let update server msg model =
    Fable.Core.JS.console.log($"{msg}")

    match msg with

    | SetPage p -> { model with Page = p }, Cmd.none

    | SignedOut ->
        { model with Session = None; Page = Login }, Cmd.none

    | SignedIn user ->
        { model with Session = Session(server,user) |> Some; Page = Chat }, Cmd.none

let view() =
    let server = new Server()

    let model, dispatch = () |> Store.makeElmish init (update server) ignore

    let unsub = server.User.Subscribe( function
        Some u -> dispatch (SignedIn u)| None -> dispatch SignedOut
    )

    Html.div [
        disposeOnUnmount [ unsub; model; server ]

        Attr.className "container"
        Attr.style [ Css.padding (rem 2) ]

        Bind.el( model .> (fun m -> (m.Page, m.Session)),
            function
            | Chat, Some session ->
                Chat.view session
            | Login, _ | _, _ ->
                Login.view server
        )
    ]

view() |> Program.mountElement "sutil-app"
