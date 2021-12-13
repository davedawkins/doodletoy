module Help

open Sutil
open Types
open Server
open Sutil.Styling
open Sutil.DOM
open Fetch
open Fable.Formatting.Markdown
open type Feliz.length

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
    rule "pre>code" [
        Css.fontFamily "monospace"
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