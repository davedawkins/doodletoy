module Help

open Sutil
open Types
open Server
open Sutil.Styling
open Sutil.DOM
open Fetch
open Fable.Formatting.Markdown
open type Feliz.length
open type Feliz.borderStyle

let parsed md =
    try
        let doc  = Markdown.Parse(md)
        let html = Markdown.ToHtml(doc)
        html
    with
        | x -> $"<pre>{x}</pre>"

let urlBase = ""//"https://raw.githubusercontent.com/davedawkins/Sutil/main/src/App"

let markdownStyle = [

    rule "li" [
        Css.listStyleTypeDisc
    ]

    rule "li::marker" [
        Css.color "#a780ff"
    ]


    rule "pre" [
        Css.backgroundColor "#ebf0f7"
        Css.color "#4a4a4a"
        Css.fontSize (em 0.875)
        Css.overflowXAuto
        Css.padding (rem 1.25, rem 1.5)
        Css.whiteSpacePre
        Css.wordWrapNormal
    ]

    rule "p>code,li>code" [
        Css.fontFamily "monospace"
        Css.border(px 1, solid, "#dddddd")
        Css.backgroundColor("#ebf0f7")
        Css.padding (px 1, px 4)
        Css.borderRadius(px 4)
    ]

    rule "pre>code" [
        Css.fontFamily "monospace"
    ]


    rule "thead th" [
        Css.borderBottom( px 1, solid, "#dddddd" )
    ]

    rule "th, td" [
        Css.paddingLeft (rem 0.5)
        Css.paddingRight (rem 0.5)
    ]

    rule "h1,h2,h3,h4,h5,h6,ul" [
        Css.marginTop (rem 1)
    ]

    rule "li,p" [
        Css.marginTop (rem 0.5)
    ]

]


let fetchSource tab  =
    //let url = sprintf "%s%s" urlBase tab
    fetch tab []
    |> Promise.bind (fun res -> res.text())

let pageView source =
    let content = Store.make "Loading.."

    fetchSource ("doc/" + source) |> Promise.map (Store.set content) |> ignore

    Html.div [
        disposeOnUnmount [ content ]
        Html.div [
            Html.span [
                Bind.el(content,fun t ->
                    html $"{parsed t}"
                        //|> postProcess addReplButtons
                        )
            ] |> withStyle markdownStyle
        ]
    ]


let view (server :Server) =
    pageView "help.md"