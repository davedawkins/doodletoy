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
let private adminTeamId = "619d6583db1da"
let private configurationCollectionId = "619dfd44d6fb5"
let private databaseId = "default"

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

    let client = Appwrite.Client.Create()
    let account = Appwrite.Account.Create(client)
    let teams = Appwrite.Teams.Create(client)
    let db = Appwrite.Databases.Create(client)
    let perm = Appwrite.Permission.Create()

    let allowAny = [|
        Appwrite.Permission.read( Appwrite.Role.any() )
        Appwrite.Permission.write( Appwrite.Role.any() )
        Appwrite.Permission.update( Appwrite.Role.any() )
        Appwrite.Permission.delete( Appwrite.Role.any() )
    |]

    let logUser (userOpt : Schema.User option) =
        userOpt |> function Some u -> JS.console.dir(u) | None -> ()
        userOpt

    let filterVisitor (userOpt : Schema.User option) =
        match userOpt with
        | Some u when
                    u.email = ""
                    //|| u.email = visitorEmail
                    ->
            None
        | Some u ->
            userOpt
        | None ->
            userOpt

    let setUser  =
        logUser >> filterVisitor //>> getTeam >> setUserStore

    let logError (msg : string) =
        JS.console.error(msg)

    let logDebug (msg : string) =
        ignore msg //JS.console.log("Server: " + msg)

    //let log (msg : string) =  () //JS.console.log(msg)

    let setSessionUser (user : Option<SessionUser>) =
        logDebug("setSessionUser " + (user |> function None -> "None"| Some u -> u.User.name))
        Store.modify (fun m -> { m with ServerModel.User = user }) model

    let setConfiguration config =
        logDebug("setConfiguration")
        Store.modify (fun m -> { m with ServerModel.Configuration = config }) model

    let startSession() = promise {
        logDebug("startSession")

        let! userOrVisitor = promise {
            try
                // We may already have an active session, so pick it up
                let! session = (account.get() : JS.Promise<Schema.User>)
                logDebug(" - resume session: " + session.email)
                return Some session
            with
            |x ->
                return None
        }

        let! isAdminTeam = promise {
            try
                let! _ = teams.get( adminTeamId )
                return true
            with _ ->
                return false
        }

        let! config = promise {
            try
                let! configResult = db.listDocuments(databaseId, configurationCollectionId) :> JS.Promise<Models.DocumentList<Configuration>>
                match configResult.total with
                | 0.0 -> return Configuration.Create()
                | _ -> return configResult.documents.[0]
            with
            | x ->
                logError("listDocuments: " + x.Message)
                return Configuration.Create()
        }

        let sessionUser =
            userOrVisitor
            //|> Some
            |> filterVisitor
            |> Option.map (fun u -> {
                SessionUser.User = u
                SessionUser.IsAdmin = isAdminTeam
            })

        setSessionUser sessionUser
        setConfiguration config

        return ()
    }

    let initSdk() =
        logDebug("initSdk")
        client
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

    static member Test() =
        promise {
            JS.console.log("## Test")

            let client = Appwrite.Client.Create()
            let account = Appwrite.Account.Create(client)

            client
                .setEndpoint(serviceUrl)
                .setProject(doodlesProjectID)
                |> ignore

            JS.console.log("- sdk, endpoint and project initialized")

            // This user doesn't exist
            let! session = account.createEmailSession( "test@test.com", "test" )

            JS.console.log("- session created") // Successful
            JS.console.log( session )

            let! user = account.get() // throws exception 401 Unauthorized

            JS.console.log("- user")
            JS.console.log( user )

            return ()
        }

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
                    let! _ = account.updateVerification(userId, secret)
                    do! this.SignOut()

                    (Ok "Email verified - please sign in") |> Verified |> dispatch
                with
                | x ->
                    (Result.Error x.Message) |> Verified |> dispatch
        }

    member _.Dispatch (msg : ExternalMessage) =
        dispatch msg

    member _.SendVerificationEmail() =
        promise {
            let! _ = account.createVerification( appUrl )
            return ()
        }

    member this.Register(email:string, password:string, name : string) =
        promise {
            let! _ = account.create( "ID.unique()", email, password, name)
            do! this.SignIn(email,password)
        }

    member _.SignIn(email:string, password:string) =
        logDebug($"SignIn: {email}")
        promise {
            try
                let! _ = account.deleteSession "current"
                logDebug("Signed out")
                ()
            with
                _ -> ()

            logDebug("createSession")
            let! _ = account.createEmailSession( email, password )

            logDebug("starting session")
            do! startSession()
        }

    member _.SignInWith(provider : string) =
        logDebug($"SignInWith: {provider}")
        account.deleteSession "current"
        |> ignoreError (fun () -> // FIXME
            account.createOAuth2Session( provider, appUrl, appUrl ) |> ignore
        )

    member _.NumLikes( t : Doodle ) : JS.Promise<int> =
        promise {
            let! likes = db.listDocuments(
                databaseId,
                likesCollectionId,
                [|
                    Appwrite.Query.equal("doodleId", t._id |> U3.Case1 |> U2.Case1)
                    //"doodleId=" + t._id
                    |] )
            return likes.documents.Length
        }

    member x.IncrementViewCount( id : string ) =
        //Fable.Core.JS.console.log("Increment view count for '" + id + "'")
        promise {
            let! allViews = x.Views(id)
            let! _ =
                if allViews.Length = 0 then
                    db.createDocument(
                        databaseId,
                        viewsCollectionId,
                        id,
                        Models.Document.Omit(Views.Create(id)),
                        allowAny
                        //([|"role:all"|], [|"role:all"|])
                    )
                else
                    let r = allViews.[0].Increment()
                    db.updateDocument( databaseId, viewsCollectionId, r._id, Models.Document.Omit(r) )

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
        promise {
            let! _ = db.deleteDocument( databaseId, likesCollectionId, l._id )
            return ()
        }

    member _.CreateLike( t: Doodle, u : User ) : JS.Promise<Like> =
        db.createDocument( databaseId, likesCollectionId, "unique()", Like.Create(t,u), allowAny )

    member _.FindLike( t: Doodle, u : User ) =
        promise {
            let! like =
                db.listDocuments(databaseId,
                    likesCollectionId,
                    [|
                        Appwrite.Query.equal("doodleId", t._id |> U3.Case1 |> U2.Case1)
                        Appwrite.Query.equal("userId", u.``$id`` |> U3.Case1 |> U2.Case1)
                        //"doodleId.equal(\"" + t._id + "\")"
                        //"userId.equal(\"" + u.``$id`` + "\")"
                    |]
                ) //: JS.Promise<ListDocumentsResult<Like>>

            if like.documents.Length = 0 then
                return None
            else
                return Some (like.documents.[0])
        }

    member _.SetFeatured( d : Doodle ) = promise {
        let data = current().Configuration.Update( featured = d._id  )
        //data.featured <- d._id
        if String.IsNullOrEmpty(data._id) then
            let! newconfig = db.createDocument(
                databaseId,
                configurationCollectionId,
                "unique()",
                data,
                [|
                    Appwrite.Permission.read(Appwrite.Role.any())
                    Appwrite.Permission.update(Appwrite.Role.team(adminTeamId, ""))
                    Appwrite.Permission.delete(Appwrite.Role.team(adminTeamId, ""))
                |]
                //[| "role:all" |], [| $"team:{adminTeamId}"|]
            )
            setConfiguration(newconfig)
        else
            let! _ = db.updateDocument(
                databaseId,
                configurationCollectionId,
                data._id,
                data,
                [|
                    Appwrite.Permission.read(Appwrite.Role.any())
                    Appwrite.Permission.update(Appwrite.Role.team(adminTeamId, ""))
                    Appwrite.Permission.delete(Appwrite.Role.team(adminTeamId, ""))
                |]
            )
            setConfiguration(data)
    }

    member _.SignOut() =
        promise {
            try
                let! _ = account.deleteSession "current"
                ()
            with x ->
                logError("SignOut: " + x.Message)
                ()
            do! startSession()
            return ()
        }

    member x.ListAll<'T when 'T :> Models.Document>( collectionId : string, filter : string array ) : JS.Promise<'T array> =
        promise {
            // let mutable chunks : ('T array) list = []
            // let mutable received = 0
            // let mutable total = 999 // Yuck. Allow initial iteration (received < total)

            // while received < total do
            //     let! chunk = db.listDocuments(databaseId, collectionId, filter, 25.0, float received) //: JS.Promise<ListDocumentsResult<'T>>

            //     if (received = 0) then
            //         total <- int(chunk.total)

            //     received <- received + chunk.documents.Length
            //     chunks <- chunk.documents :: chunks

            // return chunks |> Array.concat
            let! docs = db.listDocuments(databaseId, collectionId, filter)
            return docs.documents
        }

    member x.AllLikes() : JS.Promise<Like array>=
        x.ListAll<Like>( likesCollectionId, [| |])

    member x.AllViews() : JS.Promise<Views array> =
        x.ListAll<Views>( viewsCollectionId, [| |])

    member x.Likes( d : Doodle) =
        x.ListAll<Like>( likesCollectionId,
            [|
                Appwrite.Query.equal( "doodleId", d._id |> U3.Case1 |> U2.Case1 )
                //"doodleId.equal(\"" + d._id + "\")"
            |]
        )
        //let likes = db.listDocuments( likesCollectionId, [| "doodleId=" + d._id |] ) : JS.Promise<ListDocumentsResult<Like>>
        //likes |> Promise.map (fun r -> r.documents)

    member x.Views( id : string ) : JS.Promise<Views[]> =
        x.ListAll<Views>(
            viewsCollectionId,
            [|
                Appwrite.Query.equal( "doodleId", id |> U3.Case1 |> U2.Case1 )
                //"doodleId.equal(\"" + id + "\")"
            |] )
        //let views = db.listDocuments( viewsCollectionId, [| "doodleId=" + id |] ) : JS.Promise<ListDocumentsResult<Views>>
        //views |> Promise.map (fun r -> r.documents)

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
        db.deleteDocument(databaseId, doodlesCollectionID,id)

    member _.GetDoodle( id : string ) =
        db.getDocument(databaseId, doodlesCollectionID,id)

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

    member x.AllDoodles() =
        x.ListAll(doodlesCollectionID, [| |])
    // promise {
    //         let! doodles = db.listDocuments(doodlesCollectionID) : JS.Promise<ListDocumentsResult<Doodle>>

    //         //doodlesById <- doodles.documents |> Array.map (fun d -> d._id,d) |> Map.ofArray

    //         return doodles.documents
    // }

    member x.UserDoodles ( id : string ) : JS.Promise<DoodleView array> =
        promise {
            let! doodles =
                x.ListAll<Doodle>(
                    doodlesCollectionID,
                    [|
                        Appwrite.Query.equal( "ownedBy", id |> U3.Case1 |> U2.Case1 )
                        //"ownedBy.equal(\"" + id + "\")"
                    |]
                )

            let! result = doodles |> Array.map x.GetCachedDoodleView |> Promise.all
            return result
        }

    member x.AllDoodleViews() = promise {
            let! doodles = x.AllDoodles()

            let! likes = x.AllLikes()

            let! views = x.AllViews()

            let likeMap = likes |> Array.groupBy (fun like -> like.doodleId) |> Map.ofArray
            let viewsMap = views |> Array.groupBy (fun view -> view.doodleId) |> Map.ofArray

            //Fable.Core.JS.console.dir(views)


            let myLike (likes : Like array) =
                x.SessionUser
                |> Option.map( fun (u:SessionUser) -> likes |> Array.tryFind (fun like -> like.userId = u.User.``$id``) )
                |> Option.defaultWith (fun _ -> None)

            let createView (d : Doodle) =
                let likes' = if likeMap.ContainsKey(d._id) then likeMap.[d._id] else [| |]
                let views' = if viewsMap.ContainsKey(d._id) then viewsMap.[d._id] else [| |]

                //Fable.Core.JS.console.log("-- " + d.name + " " + d._id + " ----------------")
                //Fable.Core.JS.console.dir(views')

                {
                    Doodle = d
                    Likes = likes'
                    Views = views'
                    MyLike = myLike(likes')
                    IsFeatured = false
                }

            let dvs =
                doodles
                |> Array.map createView
                //|> Promise.all

            return dvs
    }

    member x.UpdateCreate( id : string, d : Doodle ) : JS.Promise<Doodle> =
        promise {

            d.``$id`` <- Unchecked.defaultof<_>

            let! saved =
                if String.IsNullOrEmpty(id) then
                    db.createDocument(databaseId, doodlesCollectionID, "unique()", Models.Document.Omit(d), allowAny)
                else
                    db.updateDocument(databaseId, doodlesCollectionID, id, Models.Document.Omit(d), allowAny )

            let! view = x.GetCachedDoodle(saved.``$id``)
            doodlesById <- doodlesById.Add(saved.``$id``, { view with Doodle = saved })

            return saved
        }

    member _.Iter ( f : Databases.Databases -> unit ) =
        db |> f

    member _.Map<'T>( f : Databases.Databases -> 'T ) =
        db |> f

    static member DateTimeNow =
        Math.Truncate(double(DateTime.UtcNow.Ticks) / double(TimeSpan.TicksPerSecond))

    interface IDisposable with
        member _.Dispose() = dispose()

type DoodleSession(server : Server, user : User) =
    member _.User = user

    member _.Server = server

    // member _.Post(message: string) =
    //     server.Map (fun db ->
    //         db.createDocument(
    //             databaseId,
    //             chatCollectionID,
    //             "unique()",
    //             {| message = message; user = user.name; ts = System.DateTime.Now.Ticks |}
    //         ))

    member this.SaveAsNew( doc : Types.Schema.Doodle ) : JS.Promise<Schema.Doodle> =
        //this.Save( { doc with ``$id`` = jsUndefined :?> string } )
        this.Save( jsUndefined :?> string, doc )

    member this.Save(doc : Types.Schema.Doodle ) = this.Save( doc._id, doc )

    member _.Save( id: string, doc : Types.Schema.Doodle ) : JS.Promise<Schema.Doodle> =
        //let dateTimeNow = Math.Truncate(double(DateTime.UtcNow.Ticks) / double(TimeSpan.TicksPerSecond))
        let isUndefined x = (x :> obj) = (None :> obj)
        let data =
            doc.Update( user._id, user.name, Server.DateTimeNow, if (doc.createdOn = 0.0 || isUndefined(doc.createdOn)) then Server.DateTimeNow else doc.createdOn )
        let id' = if doc.ownedBy = user.``$id`` then id else (jsUndefined :?> string)
        server.UpdateCreate(id', data)

    member _.UserDoodles () : JS.Promise<DoodleView array> =
        server.UserDoodles(user.``$id``)

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