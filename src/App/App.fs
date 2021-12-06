module App

open Sutil
open type Feliz.length
open AppwriteSdk
open Server
open Sutil.DOM
open Sutil.Styling
open UI
open Types
open Browser.Types

type Model = {
    Page : Page
    Session : DoodleSession option
    Doodle : Schema.Doodle
    }

type Message =
    | SignOut
    | SetPage of Page*string
    | SignedIn of User
    | SignedOut
    | External of ExternalMessage
    | Initialized
    | Confirmed of Message


module UrlParser =
    let parseSearch (location : Location) : Map<string,string> =
        match location.search.Length with
        | n when n > 1 && location.search.[0] = '?' ->
            location.search.Substring(1).Split('&')
            |> Array.fold (fun map token ->
                    match token.Split('=') with
                    | pair when pair.Length = 2 -> map.Add( pair.[0], pair.[1] )
                    | _ -> map) Map.empty
        | _ -> Map.empty

    let parseHash (location: Location) =
        let hash =
            if location.hash.Length > 1 then location.hash.Substring 1
            else ""
        if hash.Contains("?") then
            let h = hash.Substring(0, hash.IndexOf("?"))
            h, hash.Substring(h.Length+1)
        else
            hash, ""

    let parseUrl (location: Location) =
        parseHash location

    let parseBookPage (hash:string) =
        let items = hash.Split( [|'-'|], 2 )
        match items.Length with
        | 0 -> "", ""
        | 1 -> "", items.[0]
        | _ -> items.[0], items.[1]

    let parsePage(loc:Location) : Page =
        let hash, query = (parseUrl loc)
        match hash with
        |"profile" -> Profile
        |"editor" -> Editor
        |"browse" -> Browse
        |"login" -> Login
        |"verify" -> AwaitingVerification
        |"register" -> Register
        | _ -> Home

let init (server : Server) : Model * Cmd<Message> =
    let serverInit dispatch =
        server.Init(dispatch << External, UrlParser.parseSearch Browser.Dom.window.location)
        |> Promise.map (fun _ -> dispatch Initialized)
        |> ignore

    let initPage = Home
    {
        Page = initPage
        Session = None
        Doodle = Schema.Doodle.Create() },
        [ serverInit ]

let rec update (server : Server) (confirmed : bool) msg (model:Model) =
    //Fable.Core.JS.console.log($"{msg}")

    match msg with
    | Confirmed msg ->
        update server true msg model

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
        | Verified result ->
            let confirm dispatch =
                let message =
                    match result with
                    | Ok m -> "Email verified. Please sign in!"
                    | Result.Error s -> "Email verification failed: " + s

                { UI.ModalOptions.Create() with
                    Content = fun close ->
                        Html.div message
                    Buttons = [
                        ("OK", fun close -> close())
                    ]
                } |> UI.modal
            model, [ confirm ]

        | RegisteredNewAccount ->
            model, Cmd.ofMsg (SetPage (Registered,"registered new account"))

        | RegisterNewAccount ->
            model, Cmd.ofMsg (SetPage (Register,"new account"))

        | NewDoodle ->
            model, Schema.Doodle.Create() |> EditDoodle |> External |> Cmd.ofMsg

        | EditDoodle t ->
            let current = Editor.Storage.get()
            match current, confirmed with
            | Some d, false when d._id <> t._id ->
                let confirm dispatch =
                    { UI.ModalOptions.Create() with
                        Content = fun close ->
                            Html.div "You have unsaved edits"
                        Buttons = [
                            ("Resume", fun close -> close(); dispatch (Confirmed (External (EditDoodle d))))
                            ("Discard", fun close -> close(); dispatch (Confirmed msg))
                            ("Cancel", fun close -> close())
                        ]
                    } |> UI.modal
                model, [ confirm ]
            | _ ->
                if (not (Fable.Core.JsInterop.isNullOrUndefined t._id)) then
                    server.IncrementViewCount(t) |> ignore

                { model with Doodle = t }, Cmd.ofMsg (SetPage (Editor, "EditDoodle"))

    | SetPage (p,who) ->
        { model with Page = p }, Cmd.none

    | SignedOut ->
        { model with Session = None }, Cmd.ofMsg (SetPage (Home, "SignedOut"))

    | SignedIn user ->
        Fable.Core.JS.console.dir("User", user)
        if user.emailVerification then
            { model with Session = DoodleSession(server,user) |> Some }, Cmd.ofMsg (SetPage (Home,"SignedIn"))
        else
            model, Cmd.ofMsg (SetPage (AwaitingVerification,"SignedIn"))

let viewMain server (model : System.IObservable<Model>) dispatch =
    Bind.el( model, fun m ->
        match m.Session, m.Page with
        | _, Register -> Register.view server
        | _, Registered -> Verify.view true server
        | _, AwaitingVerification -> Verify.view false server
        | _, Home -> Home.view server
        | _, Browse -> Browse.view server
        | _, Editor -> Editor.view server m.Session m.Doodle
        | Some session, Profile -> Profile.view  session server
        | None, Profile -> Verify.view false server
        | _, _ -> Login.view server
    )


let view() =
    let server = new Server()

    let model, dispatch = server |> Store.makeElmish init (update server false) ignore

    //let unsubnav = Navigable.listenLocation UrlParser.parsePage (dispatch<<SetPage)

    Html.div [
        //unsubscribeOnUnmount [ unsubnav ]
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
            button:active {
                background-color: hsla(257.6, 56.9%, 20%, 0.40);
            }
            input[type=text], input[type=password] {
                padding: 0.3rem;
                border-radius: 6px;
                border: 1px solid hsla(257.6, 56.9%, 20%, 0.20);
            }
            input[type=text].error, input[type=password].error {
                border: 2px solid hsla(0, 100%, 65%, 0.6);
            }
            input[type=text].success, input[type=password].success {
                border: 2px solid hsla(118, 100%, 65%, 0.6);
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
            a, a:visited, a:hover, a:active {
              color: inherit;
            }
            a {
                color: hsla(257.6, 56.9%, 20%, 0.40);
            }
            a:hover {
                color: hsla(257.6, 56.9%, 20%, 0.60);
            }
            a:active {
                color: hsla(257.6, 56.9%, 20%, 0.80);
            }
            """
        ]

        UI.header [
            UI.UI.navLogo "doodletoy" (fun _ -> dispatch (SetPage (Home,"click logo")))
            Bind.el(server.State |> Store.map (fun s -> s.User), fun userOpt ->
                UI.nav [
                    match userOpt with
                    |Some su ->
                        let suffix = if su.User.emailVerification then "" else " (awaiting email verification)"
                        fragment [
                            UI.navLabelMuted "Welcome"
                            UI.navItem (su.User.name + suffix) (fun _ -> SetPage (Profile,"click") |> dispatch)
                            UI.navLabel ("|")
                            UI.navItem "Browse" (fun _ -> SetPage (Browse,"click") |> dispatch)
                            UI.navItem "New" (fun _ -> dispatch (External NewDoodle))
                            //Bind.el(model, fun m -> viewNav server m.Page m.Session dispatch)
                            UI.navItem "Sign Out" (fun _ -> server.SignOut() |> ignore)
                        ]
                    | _ ->
                        fragment [
                            UI.navItem "Browse" (fun _ -> SetPage (Browse,"click") |> dispatch)
                            UI.navItem "New" (fun _ -> dispatch (External NewDoodle))
                            UI.navItem "Sign In" (fun _ -> SetPage (Login,"click") |> dispatch)
                        ]
                ]
            )
        ]

        Html.div [
            Attr.className "page-content container"
            Attr.style [
                Css.padding (rem 4.5)
            ]
            viewMain server model dispatch
            //Bind.el( model, (fun m -> m.Page, m.Doodle), (fun m -> viewMain server m dispatch))
        ]
    ]

view() |> Program.mountElement "sutil-app"
