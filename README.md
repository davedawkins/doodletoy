## Writing Appwrite Apps in F#

I did a scary thing and signed up for 2021's Advent of F# without knowing exactly what it was, or what I'd produce. I'd been looking at things like Firebase, and then discovered Appwrite. I realized this would serve as a fallback topic until I thought of something much cooler, like virtual reality  or monad lasers. It turns out Appwrite is pretty cool, so here we are.

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

What I did

Logged into my existing linode server

Installed Appwrite with a single command:

```bash
docker run -it --rm \
    --volume /var/run/docker.sock:/var/run/docker.sock \
    --volume "$(pwd)"/appwrite:/usr/src/code/appwrite:rw \
    --entrypoint="install" \
    appwrite/appwrite:0.11.0
```

There is a small amount of configuration, but you're up and running very quickly.

On my development laptop:

```bash
npm install appwrite
```

Find the types node_modules/appwrite/types/sdk.d.ts

Pass this through ts2fable

A little more work is required to tweak the bindings, but ts2fable does a great job of the bulk of the translation.

- Talk about the property getter/setter thing

The Web App

I wanted in particular to explore the ability to authorize a user with (say) Github and/or Google, and then interact with other users. I wasn't sure if this would be a game, or some kind of chat, or a combination of both. I knew just those objectives would cover some interesting APIs, in particular authorization, database and subscription to events.

Initially, I took the default Fable template and made a tiny chat app that worked directly on the DOM.
I wanted to be completely independent of React / Sutil / etc while I tested the API.

Eventually I converted to Sutil, but you should be able to convert this app very easily to Fable.React or Feliz (future me may have done this already, in which case I'll put the link here.)



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