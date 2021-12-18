module Server

open System
open Fable.Core
open AppwriteSdk
open Sutil
open Types
open Types.Schema

[<Emit("undefined")>]
let jsUndefined : obj = jsNative

//let appUrl = "http://localhost:8080/"
let appUrl = "https://doodletoy.net/"

let private chatCollectionID = "617c11bb63b82"

let private serviceUrl = "https://appwrite.doodletoy.net/v1"
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
    let mutable doodlesById : Map<string,DoodleView> = Map.empty

    let initModel() = {
        User = None
        Configuration = Configuration.Create()
    }

    let model = ObservableStore.makeStore initModel ignore

    let current() = model |> Store.get

    let sdk = AppwriteSdk.Create()

    let logUser (userOpt : User option) =
        userOpt |> function Some u -> JS.console.dir(u) | None -> ()
        userOpt

    let filterVisitor (userOpt : User option) =
        match userOpt with
        | Some u when u.email = "" || u.email = visitorEmail ->
            None
        | Some u ->
            userOpt
        | None ->
            userOpt

    let setUser  =
        logUser >> filterVisitor //>> getTeam >> setUserStore

    let logError (msg : string) =
        JS.console.error(msg)

    //let log (msg : string) =  () //JS.console.log(msg)

    let setSessionUser (user : Option<SessionUser>) =
        //log("setSessionUser" + (user |> function None -> "None"| Some u -> u.User.name))
        //Fable.Core.JS.console.dir(model)
        Store.modify (fun m -> { m with ServerModel.User = user }) model

    let setConfiguration config =
        //log("setConfiguration")
        Store.modify (fun m -> { m with ServerModel.Configuration = config }) model

    let startSession() = promise {
        //Fable.Core.JS.console.log("startSession")

        let! userOrVisitor = promise {
            try
                // We may already have an active session, so pick it up
                let! session = (sdk.account.get() : JS.Promise<User>)
                //Fable.Core.JS.console.log(" - resume session: " + session.email)
                return session
            with
            |x ->
                let! session = sdk.account.createSession(visitorEmail, "doodletoy")
                //Fable.Core.JS.console.log($" no session ({x.Message}) - create visitor session")
                return! sdk.account.get()
        }

        let! isAdminTeam = promise {
            try
                do! sdk.teams.get( adminTeamId )
                return true
            with _ ->
                return false
        }

        let! config = promise {
            try
                let! configResult = sdk.database.listDocuments(configurationCollectionId) : JS.Promise<ListDocumentsResult<Configuration>>
                match configResult.sum with
                | 0 -> return Configuration.Create()
                | _ -> return configResult.documents.[0]
            with
            | x ->
                logError("listDocuments: " + x.Message)
                return Configuration.Create()
        }

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

    let dispose() =
        disposables |> List.iter (fun d -> d.Dispose())

    let addDisposable d =
        disposables <- d :: disposables

    let addUnsub unsub =
        disposables <- Helpers.disposable unsub :: disposables

    let doodlesToMapPairs (doodles : Doodle array) =
        doodles |> Array.map (fun d -> d._id,d)

    static member IsValidId (id : string) =
        not(Fable.Core.JsInterop.isNullOrUndefined(id) || id = "")

    member _.State : IObservable<ServerModel> = upcast model

    member _.SessionUser = current().User
    member _.Configuration = current().Configuration

    member this.Init( dispatchExternal : ExternalMessage -> unit, urlParams : Map<string,string> ) =
        promise {
            dispatch <- dispatchExternal
            initSdk()
            do! startSession()

            if urlParams.ContainsKey("userId") && urlParams.ContainsKey("secret") then
                // history.pushState(null, "", location.href.split("?")[0]);
                let userId, secret = urlParams.["userId"], urlParams.["secret"]

                Browser.Dom.history.pushState(null, "", Browser.Dom.window.location.href.Split('?').[0])

                try
                    do! sdk.account.updateVerification(userId, secret)
                    do! this.SignOut()

                    (Ok "Email verified - please sign in") |> Verified |> dispatch
                with
                | x ->
                    (Result.Error x.Message) |> Verified |> dispatch
        }

    member _.Dispatch (msg : ExternalMessage) =
        dispatch msg

    member _.SendVerificationEmail() =
        sdk.account.createVerification( appUrl ) : JS.Promise<unit>

    member this.Register(email:string, password:string, name : string) =
        promise {
            do! sdk.account.create(email, password, name)
            do! this.SignIn(email,password)
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
            sdk.account.createOAuth2Session( provider, appUrl, appUrl ) |> ignore
        )

    member _.NumLikes( t : Doodle ) : JS.Promise<int> =
        promise {
            let! likes = sdk.database.listDocuments( likesCollectionId, [| "doodleId=" + t._id |] )
            return likes.documents.Length
        }

    member x.IncrementViewCount( id : string ) =
        Fable.Core.JS.console.log("Increment view count for '" + id + "'")
        promise {
            let! allViews = x.Views(id)
            do! if allViews.Length = 0 then
                    sdk.database.createDocument(viewsCollectionId, Views.Create(id), [|"*"|], [|"*"|])
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
            try
                do! sdk.account.deleteSession "current"
            with x ->
                logError("SignOut: " + x.Message)
                ()
            do! startSession()
            return ()
        }

    member _.AllLikes() : JS.Promise<Like[]>=
        let likes = sdk.database.listDocuments( likesCollectionId ) : JS.Promise<ListDocumentsResult<Like>>
        likes |> Promise.map (fun r -> r.documents)

    member _.AllViews() : JS.Promise<Views[]> =
        let views = sdk.database.listDocuments( viewsCollectionId ) : JS.Promise<ListDocumentsResult<Views>>
        views |> Promise.map (fun r -> r.documents)

    member _.Likes( d : Doodle) =
        let likes = sdk.database.listDocuments( likesCollectionId, [| "doodleId=" + d._id |] ) : JS.Promise<ListDocumentsResult<Like>>
        likes |> Promise.map (fun r -> r.documents)

    member _.Views( id : string ) : JS.Promise<Views[]> =
        let views = sdk.database.listDocuments( viewsCollectionId, [| "doodleId=" + id |] ) : JS.Promise<ListDocumentsResult<Views>>
        views |> Promise.map (fun r -> r.documents)

    member x.GetDoodleView( d: string ) : JS.Promise<DoodleView> =
        promise {
            let! doodle = x.GetDoodle(d)

            return! x.GetDoodleView(doodle : Schema.Doodle)
        }

    member x.GetCachedDoodleView( d: Doodle ) =
        promise {
            if doodlesById.ContainsKey(d._id) then
                return doodlesById.[d._id]
            else
                return! x.GetDoodleView(d)
        }

    member x.GetDoodleView( d: Doodle ) =
        promise {
            let! likes = x.Likes d
            let! views = x.Views d._id
            let! myLike =
                x.SessionUser
                |> Option.map( fun u -> x.FindLike(d,u.User) )
                |> Option.defaultWith (fun _ -> promise {return None})

            let view = {
                Doodle = d
                Likes = likes
                Views = views
                MyLike = myLike
                IsFeatured = false
            }

            doodlesById <- doodlesById.Add(d._id,view)

            return view
        }

    member _.DeleteDoodle( id : string ) =
        sdk.database.deleteDocument(doodlesCollectionID,id)

    member _.GetDoodle( id : string ) =
        sdk.database.getDocument(doodlesCollectionID,id) : JS.Promise<Doodle>

    member x.GetCachedDoodle( id : string ) : JS.Promise<DoodleView> =
        promise {
            if doodlesById.ContainsKey(id) then
                return doodlesById.[id]
            else
                return! x.GetDoodleView(id)
        }

    member x.RefreshDoodleView( id : string ) =
        promise {
            let! doodle = x.GetDoodle(id)
            return! x.GetDoodleView(doodle)
        }

    member x.AllDoodles() = promise {
            let! doodles = sdk.database.listDocuments(doodlesCollectionID) : JS.Promise<ListDocumentsResult<Doodle>>

            //doodlesById <- doodles.documents |> Array.map (fun d -> d._id,d) |> Map.ofArray

            return doodles.documents
    }

    member x.UserDoodles ( id : string ) : JS.Promise<DoodleView array> =
        promise {
            let! doodles =
                sdk.database.listDocuments(
                    doodlesCollectionID,
                    [| "ownedBy=" + id |]
                ) : JS.Promise<ListDocumentsResult<Doodle>>

            let! result = doodles.documents |> Array.map x.GetCachedDoodleView |> Promise.all
            return result
        }

    member x.AllDoodleViews() = promise {
            let! doodles = x.AllDoodles()

            let! likes = x.AllLikes()

            let! views = x.AllViews()

            let likeMap = likes |> Array.groupBy (fun like -> like.doodleId) |> Map.ofArray
            let viewsMap = views |> Array.groupBy (fun like -> like.doodleId) |> Map.ofArray

            let createView d =
                {
                    Doodle = d
                    Likes = if likeMap.ContainsKey(d._id) then likeMap.[d._id] else [| |]
                    Views = if viewsMap.ContainsKey(d._id) then viewsMap.[d._id] else [| |]
                    MyLike = None
                    IsFeatured = false
                }

            let dvs =
                doodles
                |> Array.map createView
                //|> Promise.all

            return dvs
    }

    member x.UpdateCreate( d : Doodle ) : JS.Promise<Doodle> =
        promise {
            let! saved =
                if String.IsNullOrEmpty(d._id) then
                    sdk.database.createDocument(doodlesCollectionID, d, [| "*" |] ) : JS.Promise<Doodle>
                else
                    sdk.database.updateDocument(doodlesCollectionID,d._id,d, [| "*" |]) : JS.Promise<Doodle>

            let! view = x.GetCachedDoodle(saved._id)
            doodlesById <- doodlesById.Add(saved._id, { view with Doodle = saved })

            return saved
        }

    member _.Iter ( f : Appwrite -> unit ) =
        sdk |> f

    member _.Map<'T>( f : Appwrite -> 'T ) =
        sdk |> f

    static member DateTimeNow =
        Math.Truncate(double(DateTime.UtcNow.Ticks) / double(TimeSpan.TicksPerSecond))

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

    member this.SaveAsNew( doc : Types.Schema.Doodle ) : JS.Promise<Schema.Doodle> =
        this.Save( { doc with ``$id`` = jsUndefined :?> string } )

    member _.Save( doc : Types.Schema.Doodle ) : JS.Promise<Schema.Doodle> =
        //let dateTimeNow = Math.Truncate(double(DateTime.UtcNow.Ticks) / double(TimeSpan.TicksPerSecond))
        let isUndefined x = (x :> obj) = (None :> obj)
        let data =
            {  doc
                with
                    ``$id`` = if doc.ownedBy = user._id then doc._id else (jsUndefined :?> string)
                    ownedBy = user._id
                    ownedByName = user.name
                    modifiedOn = Server.DateTimeNow
                    createdOn = if (doc.createdOn = 0.0 || isUndefined(doc.createdOn)) then Server.DateTimeNow else doc.createdOn
            }

        server.UpdateCreate(data)

    member _.UserDoodles () : JS.Promise<DoodleView array> =
        server.UserDoodles(user._id)

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