module App

open Sutil
open type Feliz.length
open AppwriteSdk
open Server
open Sutil.DOM
open Sutil.Styling
open UI
open Types

type Model = {
    Page : Page
    Session : DoodleSession option
    Doodle : Schema.Doodle
    }

type Message =
    | SignOut
    | SetPage of Page
    | SignedIn of User
    | SignedOut
    | External of ExternalMessage
    | Initialized

let init (server : Server) : Model * Cmd<Message> =
    let serverInit dispatch =
        server.Init(dispatch << External)
        |> Promise.map (fun _ -> dispatch Initialized)
        |> ignore

    let initPage = Home
    {
        Page = initPage
        Session = None
        Doodle = Schema.Doodle.Create() },
        [ serverInit ]

let update (server : Server) msg (model:Model) =
    Fable.Core.JS.console.log($"{msg}")

    match msg with
    | SignOut ->
        server.SignOut() |> ignore
        model, Cmd.none

    | Initialized ->
        let subscribe dispatch =
            let unsub = (server.State |> Store.map (fun s -> s.User) |> Observable.distinctUntilChanged).Subscribe( fun sessionUserOpt ->
                match sessionUserOpt with
                | Some sessionUser ->
                    dispatch (SignedIn sessionUser.User)
                | None ->
                    dispatch SignedOut
            )
            ()

        model, [ subscribe ]

    | External m ->
        match m with
        | NewTurtle -> { model with Doodle = Schema.Doodle.Create() }, Cmd.ofMsg (SetPage Turtle)
        | EditTurtle t ->
            server.IncrementViewCount(t) |> ignore
            { model with Doodle = t }, Cmd.ofMsg (SetPage Turtle)

    | SetPage p -> { model with Page = p }, Cmd.none

    | SignedOut ->
        { model with Session = None }, Cmd.ofMsg (SetPage Home)

    | SignedIn user ->
        { model with Session = DoodleSession(server,user) |> Some }, Cmd.ofMsg (SetPage Home)

let viewMain server model dispatch =
    match model.Session, model.Page with
    | _, Home -> Home.view server
    | _, Browse -> Browse.view server
    | _, Turtle -> Turtle.view model.Session model.Doodle
    | Some session, Profile -> Profile.view  session server
    | _, _ -> Login.view server

let viewNav server page session dispatch =
    match session, page with
    | s, Home -> Home.nav server (dispatch<<External)
    | s, Browse -> Browse.nav server (dispatch<<External)
    | s, Turtle -> fragment []
    | _, _ -> fragment []

let run ( p : Fable.Core.JS.Promise<'T> ) =
    p
        |> Promise.map (fun x -> Fable.Core.JS.console.dir(x))
        |> Promise.catch (fun x -> Fable.Core.JS.console.error(x.Message))


let view() =
    let server = new Server()

    let model, dispatch = server |> Store.makeElmish init (update server) ignore

    Html.div [
        disposeOnUnmount [ model; server ]

        elAppend "head" [
            Html.style """
            html, body {
                background-color: rgb(251, 253, 239);
                height: 100%;
            }
            button {
                border: 1px solid hsla(257.6, 56.9%, 20%, 0.10);
                border-radius: 15px;
                background-color: hsla(257.6, 56.9%, 20%, 0.10);
                color: black;
                padding: 0.25rem 1rem;
                cursor: pointer;
            }
            button:hover {
                background-color: hsla(257.6, 56.9%, 20%, 0.20);
            }
            input[type=text], input[type=password] {
                padding: 0.3rem;
                border-radius: 6px;
                border: 1px solid hsla(257.6, 56.9%, 20%, 0.20);
            }
            h1 {
                font-size: 2rem
            }
            h2 {
                font-size: 1.5rem
            }
            h3 {
                font-size: 1.25rem
            }
            h4 {
                font-size: 1.2rem
            }
            """
        ]

        UI.header [
            UI.UI.navLogo "doodletoy" (fun _ -> dispatch (SetPage Home))
            Bind.el(server.State |> Store.map (fun s -> s.User), fun userOpt ->
                UI.nav [
                    match userOpt with
                    |Some su ->
                        fragment [
                            UI.navLabelMuted "Welcome"
                            UI.navItem (su.User.name) (fun _ -> SetPage Profile |> dispatch)
                            UI.navLabel ("|")
                            UI.navItem "Browse" (fun _ -> SetPage Browse |> dispatch)
                            UI.navItem "New" (fun _ -> dispatch (External NewTurtle))
                            //Bind.el(model, fun m -> viewNav server m.Page m.Session dispatch)
                            UI.navItem "Sign Out" (fun _ -> server.SignOut() |> ignore)
                        ]
                    | None ->
                        fragment [
                            UI.navItem "Browse" (fun _ -> SetPage Browse |> dispatch)
                            UI.navItem "Sign In" (fun _ -> SetPage Login |> dispatch)
                        ]
                ]
            )
        ]

        Html.div [
            Attr.className "page-content container"
            Attr.style [
                Css.padding (rem 4.5)
            ]
            Bind.el( model, (fun m -> m.Page), (fun m -> viewMain server (Store.current m) dispatch))
        ]
    ]

view() |> Program.mountElement "sutil-app"
