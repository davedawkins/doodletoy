module App

open Sutil
open type Feliz.length
open AppwriteSdk
open Server
open Sutil.DOM
open Sutil.Styling
open UI
open Types

type Page =
    | Login
    | Home
    | Chat
    | Turtle

type Model = {
    Page : Page
    UI : PageUI
    Session : Session option
}

let pageUi m = m.UI
let pageUiNav m = m.UI.Nav
let pageUiMain m = m.UI.Main
type Message =
    | SetPage of Page
    | SignedIn of User
    | SignedOut
    | InitUI

let initUI server page session =
    match page,session with
    | Chat, Some session ->
        Chat.ui session
    | Turtle, Some session ->
        Turtle.ui session
    | Home, _ ->
        PageUI.Create(Home.view server)
    | Login, _ | _, _ -> PageUI.Create(Login.view server)

let init server =
    let initPage = Home
    {
        Page = initPage
        Session = None
        UI = initUI server initPage None
    }, Cmd.none

let update server msg model =
    Fable.Core.JS.console.log($"{msg}")

    match msg with

    //| Home ->
    //    model, Cmd.ofMsg (if model.Session.IsSome then SetPage Turtle else SetPage Home)

    | SetPage p -> { model with Page = p }, Cmd.ofMsg InitUI

    | SignedOut ->
        { model with Session = None }, Cmd.ofMsg (SetPage Home)

    | SignedIn user ->
        { model with Session = Session(server,user) |> Some }, Cmd.ofMsg (SetPage Turtle)

    | InitUI ->
        { model with UI = initUI server model.Page model.Session }, Cmd.none

let view() =
    let server = new Server()

    let model, dispatch = server |> Store.makeElmish init (update server) ignore

    let unsub = server.User.Subscribe( function
        Some u -> dispatch (SignedIn u)| None -> dispatch SignedOut
    )

    Html.div [
        disposeOnUnmount [ unsub; model; server ]

        elAppend "head" [
            Html.style """
            body {
                background-color: rgb(251, 253, 239);
                height: 100vh;
            }
            """
        ]

        UI.header [
            UI.UI.navLogo "turtleToy" (fun _ -> dispatch (SetPage Home))
            Bind.el(server.User, fun userOpt ->
                UI.nav [
                    match userOpt with
                    |Some u ->
                        fragment [
                            UI.navLabel "Welcome"
                            UI.navItem (u.name) ignore
                            UI.navLabel ("|")
                            UI.navItem "Chat" (fun _ -> SetPage Chat |> dispatch)
                            UI.navItem "Turtle" (fun _ -> SetPage Turtle |> dispatch)
                            Bind.el(model .> pageUiNav, id)
                            UI.navItem "Sign Out" (fun _ -> server.SignOut())
                        ]
                    | None ->
                        UI.navItem "Sign In" (fun _ -> SetPage Login |> dispatch)
                ]
            )
        ]

        Html.div [
            Attr.className "page-content container"
            Attr.style [
                Css.padding (rem 4.5)
            ]
            Bind.el( model .> pageUiMain, id )
        ]
    ]

view() |> Program.mountElement "sutil-app"
