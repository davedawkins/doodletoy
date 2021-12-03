module Server

open System
open Fable.Core
open Fable.Core.JsInterop
open AppwriteSdk
open Sutil
open Types
open Types.Schema

[<Emit("undefined")>]
let jsUndefined : obj = jsNative

let private chatCollectionID = "617c11bb63b82"

let private serviceUrl = "https://solochimp.com/v1"
let private doodlesProjectID = "619bd8cd83fa8"
let private doodlesCollectionID = "619bd92054698"
let private likesCollectionId = "619bda17062c0"
let private viewsCollectionId = "619bda67c7d02"
let private visitorEmail = "visitor@solochimp.com"
let private adminTeamId = "619d6583db1da"
let private configurationCollectionId = "619dfd44d6fb5"
type ServerModel = {
    User : SessionUser option
    Configuration : Configuration
}
type Server() =
    let mutable disposables : IDisposable list = []
    let mutable dispatch : (ExternalMessage -> unit) = ignore

    let initModel() = {
        User = None
        Configuration = Configuration.Create()
    }

    let model = ObservableStore.makeStore initModel ignore

    let current() = model |> Store.get

    let sdk = AppwriteSdk.Create()

    //let userStore : IStore<User option> = Store.make None
    //let messages : IStore<ChatMessage> = Store.make null

    let logUser (userOpt : User option) =
        JS.console.log("LogUser")
        JS.console.dir(userOpt)
        userOpt |> function Some u -> JS.console.dir(u) | None -> ()
        userOpt

    let filterVisitor (userOpt : User option) =
        match userOpt with
        | Some u when u.email = "" || u.email = visitorEmail -> None
        | _ -> userOpt

    let setUser  =
        logUser >> filterVisitor //>> getTeam >> setUserStore

    //let safeMessages = messages |> Store.filter (not << isNull)

    let logError (msg : string) =
        JS.console.error(msg)

    let setSessionUser user =
        Store.modify (fun m -> { m with ServerModel.User = user }) model

    let setConfiguration config =
        Store.modify (fun m -> { m with ServerModel.Configuration = config }) model

    let startSession() = promise {
        Fable.Core.JS.console.log("startSession")

        let! userOrVisitor = promise {
            try
                // We may already have an active session, so pick it up
                return! (sdk.account.get() : JS.Promise<User>)
            with
            |x ->
                let! session = sdk.account.createSession(visitorEmail, "doodletoy")
                return! sdk.account.get()
        }

        let! isAdminTeam = promise {
            try
                do! sdk.teams.get( adminTeamId )
                return true
            with _ -> return false
        }

        let! configResult = sdk.database.listDocuments(configurationCollectionId) : JS.Promise<ListDocumentsResult<Configuration>>

        // if configResult.sum = 0 then
        //     let! newConfig = sdk.database.createDocument(configurationCollectionId, {
        //         featured = ""
        //     })

        let config =
            match configResult.sum with
            | 0 -> Configuration.Create()
            | _ -> configResult.documents.[0]

        let sessionUser =
            userOrVisitor |> Some |> filterVisitor
            |> Option.map (fun u -> {
                SessionUser.User = u
                SessionUser.IsAdmin = isAdminTeam
            })

        setSessionUser sessionUser
        setConfiguration config

        return ()
    }

    let initSdk() =
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

    //let subscribe() =
    //    sdk.subscribe(
    //        !^ $"collections.{chatCollectionID}.documents",
    //        fun r -> Store.set messages (r.payload :> ChatMessage)
    //    )

    let dispose() =
        disposables |> List.iter (fun d -> d.Dispose())

    let addDisposable d =
        disposables <- d :: disposables

    let addUnsub unsub =
        disposables <- Helpers.disposable unsub :: disposables

    // Initialize

    do
        //subscribe() |> addUnsub
        //addDisposable messages
        //addDisposable userStore
        //getSessionUser()
        ()
    // Members

    member _.State : IObservable<ServerModel> = upcast model

    member _.SessionUser = current().User
    member _.Configuration = current().Configuration

    member _.Init( dispatchExternal : ExternalMessage -> unit ) =
        dispatch <- dispatchExternal
        initSdk()
        startSession()

    member _.Dispatch (msg : ExternalMessage) =
        dispatch msg

    member _.Register(email:string, password:string, name : string) =
        promise {
            do! sdk.account.create(email, password, name)
        }

    member _.SignIn(email:string, password:string) =
        promise {
            try     sdk.account.deleteSession "current" |> ignore
            with    _ -> ()

            let! _ = sdk.account.createSession(email,password)

            do! startSession()
        }

    member _.SignInWith(provider : string) =
        sdk.account.deleteSession "current"
        |> ignoreError (fun () -> // FIXME
            sdk.account.createOAuth2Session( provider, "http://localhost:8080/", "http://localhost:8080/") |> ignore
        )

//    member _.Messages : IObservable<ChatMessage> =
//        safeMessages

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
            match x.SessionUser with
            | None ->
                return ()
            | Some su ->
                let! maybeLike = x.FindLike( d, su.User )
                match maybeLike with
                | None ->
                    let! like = x.CreateLike(d, su.User)
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

    member _.SetFeatured( d : Doodle ) = promise {
        let data = { current().Configuration with featured = d._id }
        if String.IsNullOrEmpty(data._id) then
            let! newconfig = sdk.database.createDocument(configurationCollectionId, data, [| "*" |], [| $"team:{adminTeamId}"|] )
            setConfiguration(newconfig)
        else
            do! sdk.database.updateDocument(configurationCollectionId,data._id,data, [| "*" |], [| $"team:{adminTeamId}"|] )
            setConfiguration(data)

    }

    member _.SignOut() =
        promise {
            try     sdk.account.deleteSession "current" |> ignore
            with    _ -> ()
            do! startSession()
            return ()
        }

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
                x.SessionUser
                |> Option.map( fun u -> x.FindLike(d,u.User) )
                |> Option.defaultWith (fun _ -> promise {return None})

            return {
                Doodle = d
                Likes = likes
                Views = views
                MyLike = myLike
                IsFeatured = false
            }
        }

    member _.DeleteDoodle( id : string ) =
        sdk.database.deleteDocument(doodlesCollectionID,id)

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
    member _.User = user

    member _.Server = server

    member _.Post(message: string) =
        server.Map (fun sdk ->
            sdk.database.createDocument(
                chatCollectionID,
                {| message = message; user = user.name; ts = System.DateTime.Now.Ticks |}
            ))

    // member _.SignOut() =
    //     server.SignOut()

    member _.Save( doc : Types.Schema.Doodle ) : JS.Promise<Schema.Doodle> =
        let dateTimeNow = Math.Truncate(double(DateTime.UtcNow.Ticks) / double(TimeSpan.TicksPerSecond))
        let isUndefined x = (x :> obj) = (None :> obj)
        let data =
            {  doc
                with
                    ``$id`` = if doc.ownedBy = user._id then doc._id else (jsUndefined :?> string)
                    ownedBy = user._id
                    ownedByName = user.name
                    modifiedOn = dateTimeNow
                    createdOn = if (doc.createdOn = 0.0 || isUndefined(doc.createdOn)) then dateTimeNow else doc.createdOn
            }
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