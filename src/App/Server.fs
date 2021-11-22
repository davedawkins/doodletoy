module Server

open System
open Fable.Core
open Fable.Core.JsInterop
open AppwriteSdk
open Sutil
open Types.Schema

let private chatCollectionID = "617c11bb63b82"


let private serviceUrl = "https://solochimp.com/v1"
let private doodlesProjectID = "619bd8cd83fa8"
let private doodlesCollectionID = "619bd92054698"
let private likesCollectionId = "619bda17062c0"
let private viewsCollectionId = "619bda67c7d02"
let private visitorEmail = "a@b.com"
type Server() =
    let mutable disposables : IDisposable list = []

    let sdk = AppwriteSdk.Create()
    let user : IStore<User option> = Store.make None
    let messages : IStore<ChatMessage> = Store.make null
    let setUser (userOpt : User option ) =
        JS.console.dir(userOpt)
        match userOpt with
        | Some u when u.email = "" || u.email = visitorEmail -> None
        | _ -> userOpt
        |> Store.set user

    let safeMessages = messages |> Store.filter (not << isNull)

    let logError (msg : string) =
        JS.console.error(msg)

    let createAnonymousSession() =
        sdk.account.createSession(visitorEmail, "doodletoy")
        |> Promise.map (fun _ ->
            JS.console.log("Logged in anonymously")
        )
        |> Promise.catch ignore
        |> ignore

    let getSessionUser()  =
        sdk.account.get()
        |> Promise.map setUser
        |> Promise.catch (fun x ->
            logError(x.Message)
            None |> setUser
            createAnonymousSession()
        )
        |> ignore

    let init() =
        sdk
            .setEndpoint(serviceUrl)
            .setProject(doodlesProjectID)
            |> ignore

    let ignoreError (f : unit -> unit) (p : JS.Promise<'T>) =
        p |> Promise.map (fun _ -> f()) |> Promise.catch (fun _ -> f()) |> ignore

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

        getSessionUser()

    // Members

    member _.SignIn(email:string, password:string) =
        sdk.account.deleteSession "current"
        |> ignoreError (fun () -> // FIXME
            sdk.account.createSession(email,password)
                |> Promise.map( fun s -> getSessionUser() )
                |> Promise.catch (fun x -> logError(x.Message))
                |> ignore)

    member _.SignInWith(provider : string) =
        sdk.account.deleteSession "current"
        |> ignoreError (fun () -> // FIXME
            sdk.account.createOAuth2Session( provider, "http://localhost:8080/", "http://localhost:8080/") |> ignore
        )
(*
    member _.SignInWithGoogle() =
        sdk.account.createOAuth2Session( "google", "http://localhost:8080/", "http://localhost:8080/", [| |]) |> ignore

    member _.SignInWithGithub() =
        sdk.account.createOAuth2Session( "github", "http://localhost:8080/", "http://localhost:8080/", [| |] ) |> ignore
*)
    member _.User : System.IObservable<User option> =
        upcast user

    member _.Messages : IObservable<ChatMessage> =
        safeMessages

    member _.NumLikes( t : Doodle ) : JS.Promise<int> =
        promise {
            let! likes = sdk.database.listDocuments( likesCollectionId, [| "doodleId=" + t._id |] )
            return likes.documents.Length
        }

    member x.IncrementViewCount( d : Doodle ) =
        promise {
            let! allViews = x.Views(d)
            do! if allViews.Length = 0 then
                    sdk.database.createDocument(viewsCollectionId, Views.Create(d), [|"*"|], [|"*"|])
                else
                    let r = allViews.[0]
                    r.Increment()
                    sdk.database.updateDocument(viewsCollectionId,r._id,r)

            return ()
        }

    member x.ToggleLike( d : Doodle ) =
        promise {
            match user |> Store.get with
            | None ->
                return ()
            | Some u ->
                let! maybeLike = x.FindLike( d, u )
                match maybeLike with
                | None ->
                    let! like = x.CreateLike(d, u)
                    return ()
                | Some like ->
                    do! x.RemoveLike(like)
                    return ()
        }

    member _.RemoveLike( l : Like ) : JS.Promise<unit> =
        sdk.database.deleteDocument( likesCollectionId, l._id ) : JS.Promise<Unit>

    member _.CreateLike( t: Doodle, u : User ) : JS.Promise<Like> =
        sdk.database.createDocument( likesCollectionId, Like.Create(t,u), [| "*" |] ) : JS.Promise<Like>

    member _.FindLike( t: Doodle, u : User ) =
        promise {
            let! like =
                sdk.database.listDocuments(
                    likesCollectionId,
                    [|
                        "doodleId=" + t._id
                        "userId=" + u._id
                    |]
                ) : JS.Promise<ListDocumentsResult<Like>>

            if like.documents.Length = 0 then
                return None
            else
                return Some like.documents.[0]
        }

    member _.SignOut() =
        sdk.account.deleteSession "current" |> catchError // FIXME
        setUser None
        createAnonymousSession()

    member _.Likes( d : Doodle) =
        let likes = sdk.database.listDocuments( likesCollectionId, [| "doodleId=" + d._id |] ) : JS.Promise<ListDocumentsResult<Like>>
        likes |> Promise.map (fun r -> r.documents)

    member _.Views( d : Doodle) : JS.Promise<Views[]> =
        let views = sdk.database.listDocuments( viewsCollectionId, [| "doodleId=" + d._id |] ) : JS.Promise<ListDocumentsResult<Views>>
        views |> Promise.map (fun r -> r.documents)

    member x.GetDoodleView( d: Doodle ) =
        promise {
            let! likes = x.Likes d
            let! views = x.Views d
            let! myLike =
                user
                |> Store.get
                |> Option.map( fun u -> x.FindLike(d,u) )
                |> Option.defaultWith (fun _ -> promise {return None})

            return {
                Doodle = d
                Likes = likes
                Views = views
                MyLike = myLike
            }
        }

    member _.GetDoodle( id : string ) =
        sdk.database.getDocument(doodlesCollectionID,id) : JS.Promise<Doodle>

    member x.RefreshDoodleView( id : string ) =
        promise {
            let! doodle = x.GetDoodle(id)
            return! x.GetDoodleView(doodle)
        }

    member x.AllDoodles() = promise {
            let! doodles = sdk.database.listDocuments(doodlesCollectionID) : JS.Promise<ListDocumentsResult<Doodle>>
            return doodles.documents
    }

    member x.AllDoodleViews() = promise {
            let! doodles = x.AllDoodles()
            let! dvs =
                doodles
                |> Array.map x.GetDoodleView
                |> Promise.all
            return dvs
    }

    member _.Iter ( f : Appwrite -> unit ) =
        sdk |> f

    member _.Map<'T>( f : Appwrite -> 'T ) =
        sdk |> f

    interface IDisposable with
        member _.Dispose() = dispose()

type DoodleSession(server : Server, user : User) =
    let mutable turtle = Doodle.Create()

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

    member _.Save( doc : Types.Schema.Doodle ) =
        let dateTimeNow = Math.Truncate(double(DateTime.UtcNow.Ticks) / double(TimeSpan.TicksPerSecond))
        let isUndefined x = (x :> obj) = (None :> obj)
        let data =
            {  doc
                with
                    ownedBy = user._id
                    ownedByName = user.name
                    modifiedOn = dateTimeNow
                    createdOn = if (doc.createdOn = 0.0 || isUndefined(doc.createdOn)) then dateTimeNow else doc.createdOn
            }
        Fable.Core.JS.console.dir( data )

        server.Map( fun sdk ->
            if String.IsNullOrEmpty(data._id) then
                sdk.database.createDocument(doodlesCollectionID, data, [| "*" |] )
            else
                sdk.database.updateDocument(doodlesCollectionID,data._id,data, [| "*" |])
        )

    member _.UserDoodles () : JS.Promise<ListDocumentsResult<Doodle>> =
        server.Map (fun sdk ->
            sdk.database.listDocuments(
                doodlesCollectionID,
                [| "ownedBy=" + user._id |]
            )
        )

    member this.Unlike (t : Doodle) =
        promise {
            let! like = server.FindLike(t,user)

            match like with
            |None -> ()
            |Some id ->
                do! server.RemoveLike id
                ()

            return ()
        }

    member this.Like (t : Doodle) =
        promise {
            let! likeOpt = server.FindLike(t,user)

            match likeOpt with
            |None ->
                let! like = server.CreateLike(t,user)
                ()
            |Some _ -> ()

            return ()
        }

