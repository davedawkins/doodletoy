module Types

open Sutil.DOM
open AppwriteSdk
open System
open Fable.Core.Util
open Fable.Core.JsInterop
open Fable.Core

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

    type User = Models.User<Models.Preferences>

    type SchemaHelper =
        static member Create<'T when 'T :> Models.Document>() =
            {|
                ``$id`` = Unchecked.defaultof<string>
            |} :> obj :?> 'T

    type Doodle =
        abstract member name : string with get, set
        abstract member description : string with get, set
        abstract member source : string with get, set
        abstract member ownedBy : string with get, set
        abstract member ownedByName : string with get, set
        abstract member createdOn : float with get, set
        abstract member modifiedOn : float with get, set
        abstract member isPrivate : bool with get, set
        inherit Models.Document

    type Configuration =
        abstract member featured : string with get, set
        inherit Models.Document

    type Like =
        abstract member doodleId: string with get, set
        abstract member userId: string with get, set
        inherit Models.Document

    type Views =
        abstract member doodleId:string with get, set
        abstract member numViews:float with get, set
        inherit Models.Document


    [<AutoOpen>]
    module Extensions =

        let create<'T when 'T :> Models.Document>() : 'T =
            !!{| ``$id`` = Unchecked.defaultof<string> |}

        [<Emit("Object.assign({}, $0, $1)")>]
        let assignNew (source: 'T) (newProps: 'R): 'T = jsNative

        type Configuration with
            member this.Update( featured : string ) : Configuration =
                assignNew this {| featured = featured |}

            static member Create() : Configuration =
                create<Configuration>().Update( featured = "" )

        type Views with
            member this.Increment() =
                assignNew this {| numViews = this.numViews + 1.0 |}

            static member Create( id : string, n : float ) : Views =
                let x = create<Views>()
                x.doodleId <- id
                x.numViews <- n
                x

            static member Create( id : string ) =
                Views.Create(id, 1.0)
            static member Increment(v : Views) =
                v.numViews <- v.numViews + 1.0

        type Like with
            static member Create( t : Doodle, u : User ) : Like =
                let x = create<Like>()
                x.doodleId <- t._id
                x.userId <- u._id
                x

        type Doodle with
            static member Create() : Doodle =
                let d : Doodle = create()
                d.description <- ""
                d.name <- MakeName.makeName()
                d.source <- Examples.templateSource
                d.ownedBy <- ""
                d.ownedByName <- ""
                d.createdOn <- 0.
                d.modifiedOn <- 0.
                d.isPrivate <- false
                d

            member this.UpdateModifiedOn( modifiedOn : float ) =
                assignNew this {| modifiedOn = modifiedOn |}

            member this.UpdateName( name : string ) =
                assignNew this {| name = name |}

            member this.UpdateSource( source : string ) =
                assignNew this {| source = source |}

            member this.UpdateDescription( description : string ) =
                assignNew this {| description = description |}

            member this.Update( ownedBy : string, ownedByName : string, modifiedOn : float, createdOn : float) =
                let d = assignNew this {| |}
                d.ownedBy <- ownedBy
                d.ownedByName <- ownedByName
                d.modifiedOn <- modifiedOn
                d.createdOn <- createdOn
                d

 type DoodleView = {
    Doodle : Schema.Doodle
    Likes : Schema.Like []
    Views : Schema.Views []
    MyLike : Schema.Like option
    IsFeatured : bool
}

type SessionUser = {
    User          : Schema.User
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
    | View of DoodleView

type ExternalMessage =
    | Verified of Result<string,string>
    | RegisteredNewAccount
    | RegisterNewAccount
    | NewDoodle
    | ResumeDoodle
    | EditDoodleId of string
    | ViewDoodleId of string
