# Writing Appwrite Apps in F#

## Outline

- Introduction
  - Deciding on a topic
  - DoodleToy

- DoodleToy 
  - What it is
  - How it's built and hosted

- Using Appwrite in Fable
  - Code examples
  - Bindings
  - Package

- Doodle Language
  - Parser combinators
  - Lambdas
  - Variables
  - Expressions
  - Control
  - Animation
  - Mouse Input

- Using Sutil
  - Stores, bindings, CSS

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



On my development laptop:

```bash
npm install appwrite
```

Find the types node_modules/appwrite/types/sdk.d.ts

Pass this through ts2fable

A little more work is required to tweak the bindings, but ts2fable does a great job of the bulk of the translation.

- Talk about the property getter/setter thing

## Appwrite Web Application

I wanted in particular to explore the ability to authorize a user with (say) Github and/or Google, and then interact with other users. I wasn't sure if this would be a game, or some kind of chat, or a combination of both. I knew just those objectives would cover some interesting APIs, in particular authorization, database and subscription to events.

Initially, I took the default Fable template and made a tiny chat app that worked directly on the DOM.
I wanted to be completely independent of React / Sutil / etc while I tested the API.

Eventually I converted to Sutil, but you should be able to convert this app very easily to Fable.React or Feliz (future me may have done this already, in which case I'll put the link here.)

- Authorization

Fun with github, google, discord. Github wanted email access explicitly set - needed to log output to diagnose. No twitter module.

- Recording views for visitors

Not logged in so no session

- Styling

Decided I could style the whole thing myself - Kevin Powell showed me many cool tips via his YT channel. No bulma.
Using a reset

- App

Decided to make use of Fable.React.DrawingCanvas. Converted it to Sutil.

Similar in style to shadertoy.com, but for turtle graphics. "turtletoy" -- alas it's been done. So that's how we ended
up with doodletoy.

- Language

Extended the simple language to handle function calls and to define lambdas

- Own hosting server

Decided to keep things simple and give appwrite its own server so that we didn't need to map any ports to accomodate the existing sutil.dev website. This would remove one extra factor to consider when debugging OAUTH2 redirect_uri issues at least.

## ts2fable

## Doodle Language

## Parser Combinators

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

