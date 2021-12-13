# Writing an Appwrite Web App in F#

## Outline

- Introduction
- DoodleToy 
- About Appwrite
- Appwrite Server
- Developing the Web Application
- Router
- F# Bindings for Appwrite
- Server API
- Domain Modeling
- Saving Documents
- Promises
- User Sessions

## Introduction

I did a scary thing and signed up for 2021's Advent of F# without knowing exactly what it was, or what I'd produce. I'd been looking at things like Firebase, and then discovered Appwrite. I realized this would serve as a fallback topic until I thought of something much cooler, like virtual reality or monad lasers. It turns out Appwrite is pretty cool, so I built https://doodletoy.net as both a broad learning exercise and my advent entry.

Here's a list of stuff I got to work on with this project:

- Appwrite
- Docker (installing Appwrite on Linode server)
- Converting TS into Fable (and making contribution to https://fable.io/ts2fable)
- Parser combinators
- Language design
- Canvas2D
- CSS styling top-to-bottom (no framework)
- Sutil (F# web framework)

## DoodleToy

Having decided on Appwrite, I needed a more fully defined project. I decided to make use of some work I did about a year ago with turtle graphics (links), and turn it into the turtle equivalent of https://shadertoy.com. I had this idea that everyone who read the article could go and create their own doodle. This would require me to store documents, have user accounts, etc; all things that would be a good test of Appwrite, and something that connect the visitors.

DoodleToy itself is an editor for generative art programs written in a custom turtle graphics language (based on my previous work, and heavily extended). You can create your own, and browser other people's doodles. To save doodles, you need to create an account. You can log in with Google, Github, Discord or register directly. 

The app is served from https://doodletoy.net, and is written in F# using [Fable](https://fable.io), [Sutil](https://sutil.dev) and the [Appwrite](https://appwrite.io) SDK. I used [ts2fable](https://fable.io/ts2fable) to create the F# bindings, and then hand-modified them. My intention is to release the bindings as nuget package Fable.Appwrite. While working on the bindings, I was able to contribute back some enhancements to ts2fable which you can see as the new option checkboxes on the [website](https://fable.io/ts2fable).

The backend for the app is an Appwrite server instance hosted on a [Linode](https://www.linode.com) server. I am very impressed with how easy it was to get this up and running. I had a few teething problems with setting up email and a few other things, but searching github, Google and the Appwrite Discord server solved everything. They're a very helpful bunch over on Discord.

## About Appwrite

Appwrite is an OSS alternative to Firebase, which you can host on your own servers. In their [own words](https://appwrite.io/):

> Appwrite is a self-hosted solution that provides developers with a set of easy-to-use and integrate REST APIs to manage their core backend needs.

Platforms like Firebase and Appwrite are a distillation of their big-brother cloud counterparts into simpler versions of:

- Authentication
- Session management
- User management
- Database
- Storage
- and more

Appwrite is particularly interesting for a couple of reasons:

- it's self-hosting. If you're already paying for a server, you can install it there
- the TypeScript API is held in a single file, simplifying the generation of the F# API

I can't claim that this is easier than just using Azure / AWS / GCP etc - I don't have enough experience. I do know that the scope of Appwrite (and probably Firebase) feels like it can be learned from end to end and understood. I'm sure it's a compromise on the vast functionality available from cloud platforms in order to make it easier to get basic projects up and running.

## Appwrite Server

Appwrite server installs with a single command [link]. There is a small amount of configuration, but you're up and running very quickly. 

```
   +-----------------------+      +--------------------------+  
   | https://doodletoy.net |      | https://solochimp.com    |
   | Ports: 80 / 443       |      | Ports: 80 / 443          |
   | Linode instance       |      | Linode instance          |
   | Serves the web app    |      | Runs the Appwrite server |
   | using nginx           |      | as a docker container    |
   +-----------------------+      +--------------------------+
   
```

I already had a [Linode](https://www.linode.com) instance and I decided to fire up another one to host the server. I probably could host it side-by-side with the app server, but I didn't want to complicate the routing of traffic through port 80/443, and I didn't want to use non-standard ports. The issue (as I see it) is that the Appwrite server wants to be on port 80/443 and I see no immediate way to configure it to route traffic for a different host so that the web app can be served by a plain Nginx process. I plan to see if this can be done.

If you use non-standard ports for Appwrite, you can use your nginx to do the routing, but then I suspect you'll get into problems with OAUTH2 redirection URLs - we use those to allow people to login via Google, Discord, Github etc.

Once installed, you can then log into the Appwrite console, and configure your projects. The Appwrite instance on solochimp has a project
named "doodletoy", and within this project are defined the documents, users, authentication methods and web platforms.

While testing, I'd suggest disabling the rate limits, otherwise you'll be locked out from testing your login code after a few attempts. It's a neat feature, and I need to re-enable it for doodletoy!

SLL certificates are provided by [Let's Encrypt](https://letsencrypt.org/), and that's all handled by the Appwrite installation automatically. 

## Developing the Web Application

The web application is written in F# with Fable, using an Elmish architecture (Model-View-Update), and [Sutil](https://sutil.dev) for the HTML and reactivity.

[Sutil](https://sutil.dev) is another one of my projects, and I wanted to see how well it could handle a real (real-ish) application. It's also good [dog-fooding](https://en.wikipedia.org/wiki/Eating_your_own_dog_food). It has very little documentation (because I am a bad bad developer), but I'm working on it, slowly.  It does seem to be up to the job. You need to think in a different way to when you're using React; you need to think in a more binding-oriented fashion. I particularly like the way it makes me think carefully about what is changing in the UI.

Mostly the app is structured the same way it would be with Elmish React. A top level app that hosts a navbar, and routes URLs to various other pages. There is no top level dispatching; each page manages its own state.

A Server class manages the interface to Appwrite, and is passed as a context to each page. 

```fs

let viewMain server (model : System.IObservable<Model>) dispatch : SutilElement =
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
```

This also includes a global dispatch function so that pages can invoke functionality on the main page. I think that if I refactored right now, this would disappear. I was using this to invoke the editor from the browser, but since the URL router was developed, this is now done with `window.location.href`:

```fs
      window.location.href <- "#edit?d=<doodle-id>"
```

(I had to stop tinkering with the code so that I had time to write this article.)

Session is essentially a `(Server * User)`. It was going to capture the Server-based API that only applied to a signed-in user. This would mean the Profile page, for example, would be safe to call anything on the Session API, without needing to pass or check the user's signed-in status. For example: Saving a doodle, retrieving the user's doodles.  The current state of the project approximates this design intention.

## Router

The router is implemented by subscribing to `window.location`, and then mapping URLs to Elmish messages, which are then dispatched:

```fs
    // Sutil provides Navigable as a helper
    let unsubnav = Navigable.listenLocation UrlParser.parseMessage dispatch
```

The mapping function:

```fs
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
```

## F# Bindings for Appwrite

I used ts2fable to create the initial set of bindings, and then hand modified the result.

One problem was that ts2fable initially was converting 

```ts
declare class Api {
    account: {
        users: (name : string, loggedIn? : boolean | undefined) => string[];
    };
}
export { Api };
```

to

```fs
type [<AllowNullLiteral>] ApiAccount =
    abstract users: (string -> (bool) option -> string[]) with get, set
```

whereas I would have preferred this output, which is a more usable interface with respect to the optional parameters:

```fs
type [<AllowNullLiteral>] ApiAccount =
    abstract users: name: string * ?loggedIn: bool -> string[]
```

(see [#429](https://github.com/fable-compiler/ts2fable/issues/429))

Additionally, I wanted `'T[]` instead of `ResizeArray<'T>` (see [#428](https://github.com/fable-compiler/ts2fable/issues/428))

I was able to tinker with ts2fable and produce the output I wanted, and since I couldn't be sure everyone would want the same
behaviour, I added options to both the command-line and the [web](https://fable.io/ts2fable) versions of ts2fable:

<img src='https://user-images.githubusercontent.com/285421/145894520-f19710d2-1e8b-4254-8b75-cb4e391d17a2.png' width='600px'>

## Server API

The Server class contains all the touchpoints with Appwrite. This is how initialization is performed:

```fs
    open AppwriteSdk

    let sdk = AppwriteSdk.Create()

    let initSdk() =
        sdk
            .setEndpoint(serviceUrl)
            .setProject(doodlesProjectID)
            |> ignore
```


Getting a list of all the doodles in the database:

```fs
    member x.AllDoodles() = promise {
            let! doodles = sdk.database.listDocuments(doodlesCollectionID) : JS.Promise<ListDocumentsResult<Doodle>>

            doodlesById <- doodles.documents |> Array.map (fun d -> d._id,d) |> Map.ofArray

            return doodles.documents
    }
```

This would have been a one-liner, but I decided to implement a cache to avoid calls back to the server.

The class `ListDocumentsResult` is a class I added to the Appwrite bindings module:

```fs
type ListDocumentsResult<'T> =
    abstract sum : int
    abstract documents : 'T[]
```

## Domain Modeling

The class `Doodle` is the F# model of the Appwrite `doodles` collection, configured in the server.

```fs
  type Doodle = {
        ``$id`` : string
        name : string
        description : string
        source : string
        ownedBy : string
        ownedByName : string
        createdOn : float
        modifiedOn : float
        isPrivate : bool
    }
    with
        static member Create() : Doodle = {
            ``$id`` = Unchecked.defaultof<_>
            name = MakeName.makeName()
            description = ""
            source = Examples.templateSource
            ownedBy = ""
            ownedByName = ""
            createdOn = 0.
            modifiedOn = 0.
            isPrivate = false }
        interface HasId
```

Here's the configuration of the collections in Appwrite:

<img src='https://user-images.githubusercontent.com/285421/145881347-2ae2729c-b877-4c9f-b967-2d12199170a8.png' width='600px'>

Each of these collections has an F# counterpart in Types.fs. 

The definition of `doodles` looks like this:

<img src='https://user-images.githubusercontent.com/285421/145881566-91fd9507-b979-4a92-a5ed-0a2658b5101c.png' width='400px'>

Note that I gave `Source` a type of `markdown`, instead `text`. If you edit a record by hand in the Appwrite console, the text input element seems to strip off the newlines, even if you didn't change that field. The editor used for `markdown` fields looks to be much more capable of handling newlines. It doesn't seem to affect the way the data is sent back and forth - I treat it as a `string`. Now that the UI is more developed, I haven't needed to edit a record in the Appwrite console, so this is a minor issue. 

You can also see the schema as a JSON document (abbreviated):

```json
{
    "name": "doodles",
    "dateCreated": 1637603616,
    "dateUpdated": 1637603933,
    "rules": [
        {
            "$id": "619bd9edaf49c",
            "$collection": "rules",
            "type": "text",
            "key": "name",
            "label": "Name",
            "default": "",
            "array": false,
            "required": true,
            "list": []
        },
        {
            "$id": "619bd9edb1447",
            "$collection": "rules",
            "type": "markdown",
            "key": "description",
            "label": "Description",
            "default": "",
            "array": false,
            "required": false,
            "list": []
        }
    ]
}
```

This would be an obvious starting point for a tool that can generate F# bindings such as the `Doodle` class above.

## Saving Documents

Saving a doodle works like this:

```fs
    // class Server
    member _.UpdateCreate( d : Doodle ) : JS.Promise<Doodle> =
        promise {
            let! saved =
                if String.IsNullOrEmpty(d._id) then
                    sdk.database.createDocument(doodlesCollectionID, d, [| "*" |] ) : JS.Promise<Doodle>
                else
                    sdk.database.updateDocument(doodlesCollectionID,d._id,d, [| "*" |]) : JS.Promise<Doodle>

            doodlesById <- doodlesById.Add(saved._id, saved)

            return saved
        }
        
    // class Session
    member this.SaveAsNew( doc : Types.Schema.Doodle ) : JS.Promise<Schema.Doodle> =
        this.Save( { doc with ``$id`` = jsUndefined :?> string } )

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
            
        server.UpdateCreate(data)
```

The lower-level function `UpdateCreate` determines whether we need to ask Appwrite to create a new document, or update an existing one. This depends on whether the `$id` field (mapped to `_id` using the `HasId` interface) is set (from a previous database fetch). 

The return result from both types of call is the stored document, and this is most interesting to us when we've created a new document, since we now know the document's `$id`, and subsequent saves will be calls to `update`.

Function `SaveAsNew` unsets the `$id` field, so that `UpdateCreate` is forced to call `createDocument`. This is how the UI implements `Save as Copy`.

Function `Save` unsets `$id` if another user owns this document, so that again `UpdateCreate` is forced to call `createDocument`. This is how you end up with your own copy of someone else's doodle, and how we only fork that doodle once we actually save it.

## Promises

Note how all these calls to Server (and Appwrite) are asynchronous in the form of a `Promise<'T>` return type. How does this work with the UI?

Here's the flow of control when we press `Save`:

User presses the button, and this dispatches a `Save of Doodle` message:

```
    Html.button [ text "Save"; Ev.OnClick (fun _ -> dispatch (Save doodle)) ]
```

The Elmish framework routes this directly to our `update` function:

```fs
let update server msg model =
    match msg with 
    | Save doodle -> model, Cmd.OfPromise.either (server.Save) doodle Saved Error
```

The `update` function handles `Save` by returning the model unchange, *and* a command that will invoke `server.Save(doodle)`. The Elmish framework performs the invoke for us and handles the result, when it finally appears. 

Note that we passed two additional arguments in the command: `Saved` and `Error`. These are Elmish messages which will be dispatched accordingly, straight back to our update function. Here it is again showing the additional handlers:

```fs
let update server msg model =
    match msg with 
    | Save doodle -> model, Cmd.OfPromise.either (server.Save) doodle Saved Error
    | Saved doodle -> { model with CurrentDoodle = doodle; IsModified = false }, Cmd.none
    | Error exn -> { model with ErrorMessage = exn.Message }, Cmd.none
```

When the doodle is successfully saved, we can update the model with server's copy (which will have the new `$id` if this was the first save), and we can also mark the model as now being "unmodified". The UI can react to this by hiding the "Doodle has been edited" message, etc.

If there was an error, we update the model with the error message from the exception. The UI can react to this with a slide-in card that shows the message in red, or displays it in a status field, etc.

The Save operation is now complete. The Promise has been threaded through the update function for us by the Elmish framework. We haven't had to do any of the Promise-plumbing we would normally do otherwise. This pattern works really well, and I find it very clean. 

## User Sessions

One thing I wanted to fully explore was authorizing users, both with OAuth2 ("sign in with Goodle") and regular registration. I wanted the site to work like shadertoy where anonymous visitors (guests) would be able to do everything except save their doodles. I also wanted no work to be lost if a user started editing a doodle anonymously then decided they wanted to save their work. Their work needed to be preserved throughout their sign-in or registration process.

Appwrite does an amazing job of making all of this very easy. In Appwrite server console, you just select which 3rd party providers you wish to support. You have to go to your own account with each provider and configure the referring application. This can be a challenge; each provider is slightly different but you soon get to recognize the relevant parts.

It's at this stage you'll be grateful you don't have any non-standard ports for your Appwrite server installation. I think I managed to do this, but when anything went wrong, this was what I suspected first and so I decided that standard ports would just remove that as a possible issue. Now that I have everything working (well...), I may go back and see what's possible. Ideally I want a single host instance with multiple `https://` domains served on standard ports, for Appwrite and for the web app. 

Once you have these set up, this is how you initiate a user session via an external provider:

```fs
   sdk.account.createOAuth2Session( provider, appUrl, appUrl ) 
```

where `provider = "google"` (e.g.) and `appUrl = "https://doodletoy.net"`. It's that easy.

If an account is already set up then you can create the session with a username and password instead:

```fs
   sdk.account.createSession(email,password)
```

For doodletoy, you can register for a new account, and after you've collected a name, email and password from the user, you call:

```fs
    sdk.account.create(email, password, name)
```

The account is created immediately, and you can sign the user in to create a session, but the session is marked `Unverified`. It's your
own business logic that determines how you handle unverified users. Doodletoy doesn't allow unverified sessions to save doodles and it hides the `Profile` page.

To start the process of verification, call 

```fs
   sdk.account.createVerification( appUrl )
```

The Appwrite server will send an email to the user's registered email address with a magic link, which is your `appUrl` but with a `secret` as a URL query parameter.

When your application initializes, you can check the URL and if the `secret` is present, send it to the Appwrite server:

```fs
   sdk.account.updateVerification(userId, secret)
```

Here are some outstanding issues with doodletoy's session management:

- I really wanted Twitter as a provider - you're probably reading this via a Tweet, and I wanted it to be super-easy for you to log in and save your Doodle.
- "Forgotten password" is not yet implemented
- For some reason, Oauth2.0 is not working on mobile for me. I may get this fixed before the article goes out, but if you're reading this, then I didn't :-(

It seems that Twitter isn't (yet?) supported as a provider. Hopefully soon.

The result:

<img src='https://user-images.githubusercontent.com/285421/145899049-8313235b-a02f-4cf9-bb2f-242d1e7d346e.png' width='600px'>

- Styling

Decided I could style the whole thing myself - Kevin Powell showed me many cool tips via his YT channel. No bulma.
Using a reset

See [Appendix A] for some details

See [Appendix B] for references

## Appendix A - Changes to generated output from ts2fable


- Change returned type to User and Session for appropriate calls
- Removed optional arguments from createDocument
- Convert "ResizeArray<string>" to "string array"
- Change definition of Error from System.Exception to empty interface
- createOauth2Session returns unit
- Change generated API from property get/set of function to overloaded function

## Appendix B - References

https://linode.com

https://fable.io/ts2fable

https://appwrite.io/docs/installation

https://appwrite.io/docs/getting-started-for-web

