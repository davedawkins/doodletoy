module Server

open System
open Fable.Core
open Fable.Core.JsInterop
open AppwriteSdk
open Sutil
open Types.Schema

let private chatCollectionID = "617c11bb63b82"
let private testProjectID = "6170675806cbd"
let private serviceUrl = "https://www.solochimp.com/v1"

type Server() =
    let mutable disposables : IDisposable list = []

    let sdk = AppwriteSdk.Create()
    let user : IStore<User option> = Store.make None
    let messages : IStore<ChatMessage> = Store.make null
    let setUser (u : User option ) =
        Store.set user u

    let safeMessages = messages |> Store.filter (not << isNull)

    let logError (msg : string) =
        JS.console.error(msg)


    let getSessionUser() : JS.Promise<User option> =
        sdk.account.get()
        |> Promise.map Some
        |> Promise.catch (fun x ->
            logError(x.Message)
            None)

    let init() =
        sdk
            .setEndpoint(serviceUrl)
            .setProject(testProjectID)
            |> ignore

    let catchError (p : JS.Promise<'T>) =
        p
            |> Promise.map (fun u ->
                JS.console.dir(u)
            )
            |> Promise.catch (fun x -> logError(x.Message))
            |> ignore


    let subscribe() =
        sdk.subscribe(
            !^ $"collections.{chatCollectionID}.documents",
            fun r -> Store.set messages (r.payload :> ChatMessage)
        )

    let dispose() =
        disposables |> List.iter (fun d -> d.Dispose())

    let addDisposable d =
        disposables <- d :: disposables

    let addUnsub unsub =
        disposables <- Helpers.disposable unsub :: disposables

    // Initialize

    do
        init()

        subscribe() |> addUnsub
        addDisposable messages
        addDisposable user

        getSessionUser().``then`` setUser |> ignore

    // Members

    member _.SignInWithGoogle() =
        sdk.account.createOAuth2Session( "google", "http://localhost:8080/", "http://localhost:8080/", [| |]) |> ignore

    member _.User : System.IObservable<User option> =
        upcast user

    member _.Messages : IObservable<ChatMessage> =
        safeMessages

    member _.SignOut() =
        sdk.account.deleteSession "current" |> catchError // FIXME
        setUser None

    member _.Iter ( f : Appwrite -> unit ) =
        sdk |> f

    member _.Map<'T>( f : Appwrite -> 'T ) =
        sdk |> f

    interface IDisposable with
        member _.Dispose() = dispose()

type Session(server : Server, user : User) =
    member _.User = user

    member _.Server = server

    member _.Post(message: string) =
        server.Map (fun sdk ->
            sdk.database.createDocument(
                chatCollectionID,
                {| message = message; user = user.name; ts = System.DateTime.Now.Ticks |}
            ))

    member _.SignOut() =
        server.SignOut()

    member _.Preferences
        with    get() : UserPrefs = null
        and     set( value : UserPrefs ) : unit = ()


