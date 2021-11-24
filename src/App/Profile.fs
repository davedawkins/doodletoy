module Profile

open Sutil
open Types
open Server
open Sutil.Styling
open AppwriteSdk
open UI
open type Feliz.length
open type Feliz.borderStyle
open Sutil.DOM
type Model = {
    Doodles : Schema.Doodle list
}

type Message =
    | GetDoodles
    | GotDoodles of Schema.Doodle[]

let init() =
    {
        Doodles = []
    }, Cmd.ofMsg GetDoodles

let update (session : DoodleSession) msg model =
    Fable.Core.JS.console.log($"{msg}")
    match msg with
    | GetDoodles ->
        model,
        Cmd.OfPromise.result (session.UserDoodles() |> Promise.map (fun r -> r.documents |> GotDoodles))
    | GotDoodles doodles ->
        Fable.Core.JS.console.dir(doodles)
        { model with Doodles = doodles |> List.ofArray }, Cmd.none

let style = [
    rule ".profile-userinfo" [
        Css.marginBottom (rem 1)
    ]

    rule ".profile-userinfo > div" [
        Css.displayFlex
    ]

    rule ".profile-userinfo label" [
        Css.fontWeightBold
        Css.displayInlineBlock
        Css.minWidth (rem 4)
    ]


    rule ".name" [
        Css.cursorPointer
    ]
    rule ".name:hover" [
        Css.fontWeightBold
    ]

    rule ".doodle-grid" [
        Css.displayGrid
        //Css.gridTemplateColumns(3, fr 1)
        Css.gridTemplateColumns( [fr 3; fr 8; fr 1; fr 3; fr 3] )
        Css.borderTop(px 1, solid, "#dddddd")
    ]

    rule ".doodle-grid>*" [
//        Css.borderBottom(px 1, solid, "#dddddd")
        Css.paddingLeft (rem 0.2)
        Css.paddingRight (rem 0.2)
    ]

    rule ".header" [
        Css.fontWeightBold
        Css.borderBottom(px 1, solid, "#dddddd")
    ]

    rule "h4" [
        Css.fontSize (rem 1.5)
        Css.marginBottom (rem 1)
    ]
]

let formatDT formatString (date : System.DateTime) = Date.Format.localFormat Date.Local.englishUK formatString date
let asDateTime n = System.DateTime(int64(n) * System.TimeSpan.TicksPerSecond)

let viewRecord (server : Server) (t : Schema.Doodle ) =
    fragment [
        Html.span [
            Attr.className "name"
            text t.name
            Ev.onClick (fun _ -> EditTurtle t |> server.Dispatch )
        ]
        Html.span t.description
        Html.span (string t.isPrivate)
        Html.span (asDateTime(t.createdOn) |> formatDT "yyyy-MM-dd hh:mm:ss")
        Html.span (asDateTime(t.modifiedOn) |> formatDT "yyyy-MM-dd hh:mm:ss")
    ]

let view (session : DoodleSession) server =
    let model , dispatch = () |> Store.makeElmish init (update session) ignore

    let header name = Html.span  [ Attr.className "header"; text name]
    Html.div [
        Attr.className "profile-container"
        Html.div [
            Attr.className "profile-userinfo"

            Html.div [
                Html.label [  text "Name:"  ]
                Html.span (text (session.User.name))
            ]
            Html.div [
                Html.label [  text "Email:"  ]
                Html.span (text (session.User.email))
            ]
        ]
        Html.div [
            Html.h4 "Your Doodles"
        ]
        UI.divc "doodle-grid" [
            fragment [
                header "Name"
                header "Description"
                header "Private"
                header "Created"
                header "Modified"
            ]
            Bind.el(model |> Store.map (fun m -> m.Doodles), fun doodles ->
                fragment [
                    for d in doodles do
                        viewRecord server d
                ]
            )
            //Bind.each( model |> Store.map (fun m -> m.Doodles), viewRecord dispatchExternal, (fun r -> r._id) )
        ]
    ] |> withStyle style