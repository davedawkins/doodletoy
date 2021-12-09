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

## Appwrite Server

Appwrite is an OSS alternative to Firebase, which you can host on your own servers.

These platforms feel like a distillation of the best bits of any cloud platform, with a focus on what you need to create a web (or mobile) app:

- Authentication
- Session management
- User management
- Database
- Storage
- and more

Appwrite is particularly interesting for a couple of reasons:

- it's self-hosting. If you're already paying for a server, you can install it there
- the TypeScript API is held in a single file, simplifying the generation of the F# API

Confessions:
I have very little experience in deploying web apps to Azure, GCP or AWS. Therefore I can't compare my experience with Appwrite to those platforms, or even to Firebase.

If you find Appwrite interesting, and you'd like to create a web app with F#, then this article should be interesting for you. This isn't a comparison of appwrite with anything else

Appwrite server installs with a single command [link]. There is a small amount of configuration, but you're up and running very quickly.

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

