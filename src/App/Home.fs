module Home

open Sutil
open Sutil.DOM
open Sutil.Bindings
open Server
open Types
open UI
open Sutil.Styling
open type Feliz.length
open AppwriteSdk

type Model =
    {
        Featured : Schema.Doodle option
    }
type Message =
    | GetFeatured of string
    | SetFeatured of Schema.Doodle option
    | External of ExternalMessage

let init (server : Server) =
    { Featured = None }, Cmd.none

let update (server : Server) msg model =
    match msg with
    | External m ->
        server.Dispatch m
        model, Cmd.none
    | SetFeatured d ->
        { model with Featured = d}, Cmd.none
    | GetFeatured id->
        model,
        Cmd.OfPromise.result (server.GetDoodle(id) |> Promise.map (fun r -> r |> (SetFeatured << Some)))


let style = [
    rule ".home-turtle" [
        Css.fontSize (percent 75)
        Css.alignItemsCenter
        Css.width (px 250)
    ]

    rule ".turtle-container" [
        Css.cursorPointer
    ]

    rule ".turtle-browser" [
        Css.displayGrid
        Css.gap (rem 2)
        Css.custom("justifyItems", "center")
        Css.custom("grid-template-columns", "repeat(auto-fill, minmax(250px, 1fr))")
    ]

    rule ".featured" [
        Css.maxWidth (vh 50)
    ]

    rule ".feature-welcome" [
        Css.displayFlex
        Css.flexDirectionColumnReverse
    ]

    Media.MinWidth( UI.BreakPoint, [
        rule ".feature-welcome" [
            Css.flexDirectionRow
            Css.gap (rem 5)
        ]
    ])

    rule "h1" [
        Css.fontSize (rem 2)
        Css.marginTop (rem 2.5)
    ]

    rule "h2" [
        Css.marginBottom (rem 1)
    ]

    rule ".home" [
        Css.displayFlex
        Css.flexDirectionColumn
        Css.gap (rem 2)
    ]

    rule ".card" [
        Css.displayGrid
        Css.padding (rem 2)
        Css.gap (rem 2)
    ]

    Media.MinWidth( UI.BreakPoint, [
        rule ".card" [
            Css.gridTemplateColumns(2,fr 1)
        ]
    ])

    rule ".card p" [
        Css.marginTop (rem 1)
    ]

    rule ".card code" [
        Css.backgroundColor "#202020"
        Css.color "#dddddd"
        Css.padding (rem 1)
    ]
    rule ".card pre" [
        Css.fontFamily "Consolas,monospace"
        Css.margin 0
    ]
    rule ".card ul" [
        Css.padding 0
        Css.margin 0
        Css.listStyleTypeNone
    ]

    rule ".card li::marker" [
        Css.color "red"
    ]

    rule ".loading" [
        Css.displayFlex
        Css.alignItemsCenter
        Css.justifyContentCenter
        Css.width (px 400)
        Css.height (px 440)
    ]

    rule ".loading i" [
        Css.fontSize (rem 8)
        Css.color "#eeeeee"
    ]

    rule ".create-and-share p" [
        Css.lineHeight (rem 1.5)
        Css.marginTop (rem 0.5)
        Css.marginBottom (rem 0.5)
    ]

    rule ".contact" [
        Css.marginTop (rem 2)
    ]
    rule ".contact>div" [
        Css.gap (rem 2)
    ]
    rule ".fa-twitter" [
        Css.color "rgb(71, 155, 233)"
        Css.marginRight (rem 0.5)
        //Css.fontSize (percent 150)
    ]
    rule ".fa-github" [
        Css.color "black"
        Css.marginRight (rem 0.5)
        //Css.fontSize (percent 150)
    ]

    rule ".view a" [
        Css.marginLeft auto
    ]
    rule ".view i" [
        Css.fontSize (percent 100)
    ]
    Media.MinWidth(UI.BreakPoint, [
        rule ".view" [
            Css.width (vh 75)
        ]
    ])
]

open Browse.DoodleView

let view (server : Server) =
    let model, dispatch = server |> Store.makeElmish init (update server) ignore

    let unsub = (server.State |> Store.map ( fun s -> s.Configuration.featured)).Subscribe( fun f ->
        match f with
        | "" -> ()
        | id -> id |> GetFeatured |> dispatch
    )

    UI.divc "home" [
        disposeOnUnmount [ unsub ]

        UI.divc "feature-welcome" [
            UI.divc "featured-container" [
                Html.h2 "Featured Doodle"
                Bind.el( model |> Store.map (fun m -> m.Featured), fun doodleOpt ->
                    let options = { Animation = Always; SizePx = 400; ShowFooter = true }
                    match doodleOpt with
                    | Some doodle ->
                        Browse.DoodleView.view server options doodle
                    | None ->
                        UI.divc "loading" [
                            Html.i [ Attr.className "fas fa-spinner fa-pulse" ]
                        ]
                )
            ]
            Html.div [
                Attr.className "create-and-share"
                Html.h1 "Create and share your own doodles, be inspired by other people's creations"
                Html.p [
                    Html.a [
                        Attr.href "#browse"
                        text "Browse"
                    ]
                    text " more doodles from other contributors"
                ]
                Html.p [
                    Html.a [ Attr.href "#new"; text "Create" ]
                    text " your own doodle, or click to modify an existing doodle"
                ]
                Html.p [
                    Html.a [ Attr.href "#signin"; text "Sign in" ]
                    text " so that you can save your doodles and share them for others to see, like and modify"
                ]

                UI.divc "contact" [
                    Html.h2 "Contact"
                    //UI.flexRow [
                    Html.p [
                        Html.a [
                            Attr.href "https://twitter.com/DaveDawkins"
                            Html.i [ Attr.className "fa fa-twitter" ]
                        ]
                        text "Find me on "
                        Html.a [
                            Attr.href "https://twitter.com/DaveDawkins"
                            text "Twitter"
                        ]
                    ]
                    Html.p [
                        Html.a [
                            Attr.href "https://github.com/davedawkins/sutil-template-appwrite"
                            Html.i [ Attr.className "fa fa-github" ]
                        ]
                        text "Find source code and log issues on "
                        Html.a [
                            Attr.href "https://github.com/davedawkins/sutil-template-appwrite"
                            text "Github"
                        ]
                    ]
                    //]
                ]
            ]
        ]

    ] |> withStyle style


let shareToTwitter( doodle : Schema.Doodle ) =
    let w = Browser.Dom.window
    let url = w.location.href
    let text = "Check my doodle out!\n"
    Browser.Dom.window.``open``(
        "https://twitter.com/share?url="+w.encodeURIComponent(url)+"&text="+w.encodeURIComponent(text),
        "",
        "left=0,top=0,width=550,height=450,personalbar=0,toolbar=0,scrollbars=0,resizable=0") |> ignore
    ()

module View =
    let view (server : Server) (doodle : Schema.Doodle)=
        let model, dispatch = server |> Store.makeElmish init (update server) ignore

        let unsub = (server.State |> Store.map ( fun s -> s.Configuration.featured)).Subscribe( fun f ->
            match f with
            | "" -> ()
            | id -> id |> GetFeatured |> dispatch
        )
        let options = { Animation = Always; SizePx = 0; ShowFooter = false }

        UI.divc "view" [
            Attr.style [
            ]
            disposeOnUnmount [ unsub ]

            Html.h2 doodle.name
            UI.flexRow [
                Attr.style [
                    Css.marginBottom (rem 0.5)
                ]
                Html.span ("by " + doodle.ownedByName)
                Html.a [
                    Attr.style [
                        Css.marginLeft auto
                        Css.fontSize (percent 100)
                    ]
                    Attr.href "#"

                    Ev.onClick (fun e -> e.preventDefault(); shareToTwitter doodle)

                    Html.i [ Attr.className "fa fa-twitter" ]
                    text " Share"
                ]
            ]
            Browse.DoodleView.view server options doodle

        ] |> withStyle style
