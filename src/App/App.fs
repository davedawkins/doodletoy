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
    }

type Message =
    | SignOut
    | SetPage of Page*string // Change page directly, no change to URL or browse history
    | SignedIn of User
    | SignedOut
    | External of ExternalMessage
    | Initialized
    | Confirmed of Message
    | SetUrl of string // Change page via URL, and add to browse history

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

    let parseQuery (query : string) : Map<string,string> =
        query.Split('&')
            |> Array.fold (fun map token ->
                    match token.Split('=') with
                    | pair when pair.Length = 2 -> map.Add( pair.[0], pair.[1] )
                    | _ -> map) Map.empty

    let parseHash (location: Location) =
        let hash =
            if location.hash.Length > 1 then location.hash.Substring 1
            else ""
        if hash.Contains("?") then
            let h = hash.Substring(0, hash.IndexOf("?"))
            h, parseQuery(hash.Substring(h.Length+1))
        else
            hash, Map.empty

    let parseUrl (location: Location) =
        parseHash location

    let parseMessage(loc:Location) : Message =
        let hash, query = (parseUrl loc)
        match hash with
        |"create" -> External NewDoodle
        |"new" -> External NewDoodle
        |"profile" -> SetPage (Profile,"navigate")
        |"edit" ->
            if query.ContainsKey("d") then
                External (EditDoodleId query.["d"])
            else
                External (EditDoodleId "")
        |"browse" -> SetPage (Browse,"navigate")
        |"logout" -> SignOut
        |"signout" -> SignOut
        |"help" -> SetPage (Help,"navigate")
        |"signin" -> SetPage (Login,"navigate")
        |"login" -> SetPage (Login,"navigate")
        |"verify" -> SetPage (AwaitingVerification,"navigate")
        |"register" -> SetPage (Register,"navigate")
        | _ -> SetPage (Home,"navigate")

    let toUrl (p : Page) : string =
        match p with
        | Editor d -> "#editor?d=" + d._id
        | _ -> "#" + (string p)

let init (server : Server) : Model * Cmd<Message> =
    let serverInit dispatch =
        server.Init(dispatch << External, UrlParser.parseSearch Browser.Dom.window.location)
        |> Promise.map (fun _ -> dispatch Initialized)
        |> ignore

    let initPage = Home
    {
        Page = initPage
        Session = None
        },
        [ serverInit ]

let rec update (server : Server) (confirmed : bool) msg (model:Model) =
    //Fable.Core.JS.console.log($"{msg}")

    match msg with

    | SetUrl url ->
        Browser.Dom.window.location.href <- url
        model, Cmd.none

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
            model, "" |> EditDoodleId |> External |> Cmd.ofMsg

        | EditDoodleId id ->
            let current = Editor.Storage.get()
            match current, confirmed with
            | Some d, false when d._id <> id ->
                let confirm dispatch =
                    { UI.ModalOptions.Create() with
                        Content = fun close ->
                            Html.div ("You have unsaved edits for " + d.name)
                        Buttons = [
                            ("Resume", fun close -> close(); dispatch (Confirmed (External (EditDoodleId d._id))))
                            ("Discard", fun close -> close(); dispatch (Confirmed msg))
                            ("Cancel", fun close -> close())
                        ]
                    } |> UI.modal
                model, [ confirm ]
            | _ ->
                if (not (Fable.Core.JsInterop.isNullOrUndefined id)) then
                    server.IncrementViewCount(id) |> ignore

                let getOrCreate id =
                    promise {
                        if id = "" then
                            return Schema.Doodle.Create()
                        else
                            return! server.GetCachedDoodle(id)
                    }

                model,
                    Cmd.OfPromise.perform getOrCreate id (fun doodle -> SetPage (Editor doodle, "EditDoodle"))
                    //Cmd.ofMsg (SetPage (Editor (getOrCreate(id)), "EditDoodle"))

    | SetPage (p,who) ->
        { model with Page = p }, Cmd.none

    | SignedOut ->
        { model with Session = None }, Cmd.ofMsg (SetUrl "#home")

    | SignedIn user ->
        if user.emailVerification then
            { model with Session = DoodleSession(server,user) |> Some }, Cmd.ofMsg (SetUrl "#home")
        else
            model, Cmd.ofMsg (SetPage (AwaitingVerification,"SignedIn"))

let viewMain server (model : System.IObservable<Model>) dispatch =
    Bind.el( model, fun m ->
        match m.Session, m.Page with
        | _, Register -> Register.view server
        | _, Registered -> Verify.view true server
        | _, AwaitingVerification -> Verify.view false server
        | _, Home -> Home.view server
        | _, Help -> Help.view server
        | _, Browse -> Browse.view server
        | _, Editor d -> Editor.view server m.Session d
        | Some session, Profile -> Profile.view  session server
        | None, Profile -> Verify.view false server
        | _, _ -> Login.view server
    )

let appStyle = [

    rule ".page-content" [
        Css.padding (rem 1)
    ]

    Media.MinWidth( UI.BreakPoint, [
        rule ".page-content" [
            Css.paddingTop (rem 2)
            Css.paddingBottom (rem 1)
            Css.paddingLeft (rem 4.5)
            Css.paddingRight (rem 4.5)
        ]
    ])
]

let view() =
    let server = new Server()

    let model, dispatch = server |> Store.makeElmish init (update server false) ignore

    let unsubnav = Navigable.listenLocation UrlParser.parseMessage dispatch

    Html.div [
        unsubscribeOnUnmount [ unsubnav ]
        disposeOnUnmount [ model; server ]

        UI.header [
            UI.UI.navLogo "doodletoy" "#home"
            Bind.el(server.State |> Store.map (fun s -> s.User), fun userOpt ->
                UI.nav [
                    match userOpt with
                    |Some su ->
                        let suffix = if su.User.emailVerification then "" else " (awaiting email verification)"
                        fragment [
                            UI.navLabelMuted "Welcome"
                            UI.navUrl (su.User.name + suffix) "#profile"
                            UI.navLabelIfWide ("|")
                            UI.navUrl "Browse" "#browse"
                            UI.navUrl "New" "#new"
                            UI.navUrl "Help" "#help"
                            UI.navUrl "Sign Out" "#signout"
                        ]
                    | _ ->
                        fragment [
                            UI.navUrl "Browse" "#browse"
                            UI.navUrl "New" "#new"
                            UI.navUrl "Help" "#help"
                            UI.navUrl "Sign In" "#login"
                        ]
                ]
            )
        ]

        Html.div [
            Attr.className "page-content container"
            viewMain server model dispatch
        ]
    ] |> withStyle appStyle

view() |> Program.mountElement "sutil-app"
