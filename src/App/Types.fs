module Types

open Sutil.DOM
open AppwriteSdk
type Page =
    | Login
    | Profile
    | Browse
    | Home
    | Chat
    | Turtle

module Schema =
    [<AllowNullLiteral>]
    type ChatMessage =
        abstract ts: int
        abstract message: string
        abstract user: string

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
            name = ""
            description = ""
            source = Examples.templateSource
            ownedBy = ""
            ownedByName = ""
            createdOn = 0.
            modifiedOn = 0.
            isPrivate = false }
        interface HasId

    type Configuration = {
        ``$id`` : string
        featured : string }
    with
        static member Create() = { ``$id`` = Unchecked.defaultof<_>; featured = "" }
        interface HasId
    type Like = {
        doodleId: string
        userId: string
    }
    with
        static member Create( t : Doodle, u : User ) : Like = {
            doodleId = t._id
            userId = u._id
            }
        interface HasId

    type Views = {
        doodleId: string
        mutable numViews: float
    }
    with
        static member Create( id : string, n : float ) : Views = {
                doodleId = id
                numViews = n
            }
        static member Create( t : Doodle ) =
            Views.Create(t._id, 1.0)

        member x.Increment() =
            x.numViews <- x.numViews + 1.0

        interface HasId

type DoodleView = {
    Doodle : Schema.Doodle
    Likes : Schema.Like []
    Views : Schema.Views []
    MyLike : Schema.Like option
    IsFeatured : bool
}

type SessionUser = {
    User          : User
    IsAdmin       : bool
}

type ExternalMessage =
    | NewTurtle
    | EditTurtle of Schema.Doodle
