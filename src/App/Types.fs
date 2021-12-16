module Types

open Sutil.DOM
open AppwriteSdk
open System

module MakeName =
    let nouns = [|
        "apple"
        "apricot"
        "avocado"
        "banana"
        "bellPepper"
        "bilberry"
        "blackberry"
        "blackCurrant"
        "bloodOrange"
        "blueberry"
        "boysenberry"
        "breadfruit"
        "canary melon"
        "cantaloupe"
        "cherimoya"
        "cherry"
        "chiliPepper"
        "clementine"
        "cloudberry"
        "coconut"
        "cranberry"
        "cucumber"
        "currant"
        "damson"
        "date"
        "dragonfruit"
        "durian"
        "eggplant"
        "elderberry"
        "feijoa"
        "fig"
        "gojiberry"
        "gooseberry"
        "grape"
        "grapefruit"
        "guava"
        "honeydew"
        "huckleberry"
        "jackfruit"
        "jambul"
        "jujube"
        "kiwi"
        "kumquat"
        "lemon"
        "lime"
        "loquat"
        "lychee"
        "mandarine"
        "mango"
        "mulberry"
        "nectarine"
        "nut"
        "olive"
        "orange"
        "papaya"
        "passionfruit"
        "peach"
        "pear"
        "persimmon"
        "physalis"
        "pineapple"
        "plum"
        "pomegranate"
        "pomelo"
        "mangosteen"
        "quince"
        "raisin"
        "rambutan"
        "raspberry"
        "redcurrant"
        "rockMelon"
        "salal berry"
        "satsuma"
        "starfruit"
        "strawberry"
        "tamarillo"
        "tangerine"
        "tomato"
        "watermelon"
        |]
    let adjectives = [|
        "big"
        "scary"
        "fuzzy"
        "whizzy"
        "wonky"
        "spinning"
        "cloudy"
        "sunny"
        "rainy"
        "glowing"
        "freezing"
        "hairy"
        "lively"
        "electric"
        "arcadian"
        "cagey"
        "cerulean"
        "capricious"
        "dapper"
        "spiffy"
        "modest"
        "exultant"
        "jocular"
        "luminous"
        "munificent"
        "redolent"
        "sagacious"
        "succint"
        "tenacious"
        "verdant"
        "quiet"
        "noisy"
        "bouncy"
        "super"
        "hyper"
        "instant"
    |]
    let shuffleG xs = xs |> Seq.sortBy (fun _ -> Guid.NewGuid())
    let makeName() =
        let a = adjectives |> shuffleG |> Seq.head
        let b = nouns |> shuffleG |> Seq.head
        let capFirst (x : string) = x.Substring(0,1).ToUpper() + x.Substring(1)
        (capFirst a) + (capFirst b)
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
            name = MakeName.makeName()
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
        static member Create( id : string ) =
            Views.Create(id, 1.0)

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

type Page =
    | Login
    | Register
    | AwaitingVerification
    | Registered
    | Profile
    | Browse
    | Home
    | Help
    | Editor of Schema.Doodle
    | Chat
    | View of Schema.Doodle

type ExternalMessage =
    | Verified of Result<string,string>
    | RegisteredNewAccount
    | RegisterNewAccount
    | NewDoodle
    | EditDoodleId of string
    | ViewDoodleId of string
