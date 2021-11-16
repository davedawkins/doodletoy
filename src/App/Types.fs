module Types

open Sutil.DOM

type PageUI = {
        Main : SutilElement
        Nav : SutilElement
    }
    with
        static member Create(main) = { Main = main; Nav = fragment [] }
        static member Create(main,nav) = { Main = main; Nav = nav }


module Schema =
    [<AllowNullLiteral>]
    type ChatMessage =
        abstract ts: int
        abstract message: string
        abstract user: string

    [<AllowNullLiteral>]
    type TurtleDoc =
        abstract id : int
        abstract name : string
        abstract source : string
        abstract ownedBy : string
        abstract likedBy : string []
        abstract numViews : int

    [<AllowNullLiteral>]
    type UserPrefs =
        abstract currentTurtle : string