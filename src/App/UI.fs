module UI

open Sutil
open Sutil.Html
open Sutil.DOM
open Sutil.Styling
open type Feliz.length

open Fable.Core.JsInterop

let headerStyle = [
    let textColor = "rgb(251, 253, 239)"

    let bgColor="#281651"
    let bgColorLight="rgb(247,251,251)"

    rule "header" [
        Css.displayFlex
        Css.flexDirectionRow
        Css.justifyContentSpaceBetween
        Css.backgroundColor "#281651"
        Css.color textColor
        Css.paddingLeft (rem 1)
        Css.paddingRight (rem 1)
        //Css.fontWeightBold
    ]

    rule "header a" [
        Css.color textColor
        Css.textDecorationNone
        Css.height (percent 85)
        Css.width (percent 100)
        Css.displayFlex
        Css.alignItemsCenter
        //Css.justifyContentCenter
    ]

    rule "header .ui-nav-logo" [
        Css.fontSize (rem 2)
    ]

    rule ".ui-nav-menu" [
        Css.displayFlex
        Css.flexDirectionRow
        Css.height (percent 100)
        Css.gap (rem 1)
    ]

    rule ".ui-nav-item" [
        Css.displayFlex
        Css.height (percent 100)
        Css.alignItemsCenter
    ]

    rule ".ui-nav-dropdown" [
        Css.displayFlex
        Css.height (percent 100)
        Css.alignItemsCenter
        Css.positionRelative
    ]

    rule ".ui-nav-dropdownitems" [
        Css.positionAbsolute
        Css.left (rem -0.5)
        Css.right (rem -0.5)
        Css.paddingLeft (rem 0.5)
        Css.paddingRight (rem 0.5)
        Css.paddingBottom (rem 0.5)
        Css.top (percent 100)
        Css.zIndex 20
        Css.backgroundColor bgColor
        Css.transformScaleY 0
        Css.custom("transform-origin", "top")
        Css.transition "transform ease-in-out 200ms"
    ]

    rule ".ui-nav-dropdownitems .ui-nav-item" [
        Css.opacity 0.
        Css.transition "opacity ease-in-out 200ms"
    ]

    rule ".ui-nav-dropdown:hover .ui-nav-dropdownitems" [
        Css.transition "transform ease-in-out 200ms"
        Css.transformScaleY 1
    ]

    rule ".ui-nav-dropdown:hover .ui-nav-item" [
        Css.transition "opacity ease-in-out 100ms 100ms"
        Css.opacity 1.
    ]

    rule ".ui-nav-menu a:hover" [
        Css.color "yellow"
        //Css.backgroundColor "gray"
    ]

    rule ".ui-nav-dropdown:hover" [
        //Css.fontWeightBold
    ]

    rule ".page-content" [
        //Css.backgroundColor bgColor
    ]

    rule ".ui-muted" [
        Css.color "darkgray"
    ]
]

type Close = unit -> unit
type ModalOptions = {
        ShowCancel : bool
        OnCancel : unit -> unit
        Buttons : (string * (Close -> unit)) list
        Content : Close -> SutilElement
    } with
    static member Create() =
        { ShowCancel = true; OnCancel = ignore; Buttons = []; Content = fun _ -> Html.div [] }
    static member Create( content : (unit->unit) -> SutilElement) =
        { ShowCancel = true; OnCancel = ignore; Buttons = []; Content = content }

type UI =
    static member flexColumn (children:SutilElement seq) =
        Html.div (
            [ Attr.styleAppend [ Css.displayFlex; Css.flexDirectionColumn ] ]
            |> Seq.append children)

    static member flexRow (children:SutilElement seq) =
        Html.div (
            [ Attr.styleAppend [ Css.displayFlex; Css.flexDirectionRow ] ]
            |> Seq.append children)

    static member divc (cls:string) (items : seq<SutilElement>) =
        Html.div [ Attr.className cls ; yield! items ]

    static member alinkc (cls:string) (items : seq<SutilElement>) =
        Html.a [ Attr.className cls ; yield! items ]

    static member navItem label click =
        UI.divc "ui-nav-item" [ Html.a [ Attr.href "#"; text label; Ev.onClick (fun e -> e.preventDefault(); click())] ]

    static member navLabel label =
        UI.divc "ui-nav-item" [ text label ]

    static member navLabelMuted label =
        UI.divc "ui-nav-item ui-muted" [ text label ]

    static member navLogo label click =
        UI.divc "ui-nav-logo" [
            UI.navItem label click
        ]

    static member navDropdown label items =
        UI.divc "ui-nav-dropdown" [
            Html.label [
                text label
            ]
            UI.divc "ui-nav-dropdownitems" [
                yield! items
            ]
        ]

    static member nav items =
        Html.nav [
            UI.divc "ui-nav-menu" items
        ]

    static member header items =
        Html.header [
            yield! items
        ] |> withStyle headerStyle

    static member modal (options : ModalOptions)=
        let doc = Browser.Dom.document
        let lastBodyElement : Browser.Types.HTMLElement = doc.body?lastElementChild
        let close() = DOM.unmount (doc.querySelector("#ui-modal"))
        let modalBg =
            Html.div [
                Attr.id "ui-modal"
                Attr.style [
                    Css.positionFixed
                    Css.displayFlex
                    Css.justifyContentCenter
                    Css.alignItemsCenter
                    Css.backgroundColor "rgba(0,0,0,0.65)"
                    Css.left (px 0)
                    Css.right (px 0)
                    Css.top (px 0)
                    Css.bottom (px 0)
                    Css.zIndex 10
                ]
                Html.div [
                    Attr.style [
                        Css.displayFlex
                        Css.flexDirectionColumn
                        Css.padding (rem 1.5)
                        Css.positionRelative
                        Css.backgroundColor "white"
                        Css.borderRadius (px 4)
                        Css.gap (rem 1)
                        Css.alignItemsCenter
                    ]
                    if (options.ShowCancel) then
                        Html.div [
                            Attr.style [
                                Css.positionAbsolute
                                Css.top (rem 0)
                                Css.right (rem 0)
                                Css.cursorPointer
                                Css.transformRotateZ(45.0)
                                Css.fontWeightBold
                                Css.fontSize (rem 2)
                                Css.lineHeight (rem 1.5)
                                Css.filterDropShadow(0,0,8,"#505050")
                            ]
                            text "+"
                            Ev.onClick (fun _ -> close())
                        ]

                    (options.Content) close

                    if not options.Buttons.IsEmpty then
                        Html.div [
                            Attr.style [
                                Css.displayFlex
                                Css.gap (rem 1)
                            ]
                            for (label,click) in options.Buttons do
                                Html.button [
                                    text label
                                    Ev.onClick (fun _ -> click(close))
                                ]
                        ]
                ]
            ]
        Sutil.DOM.mountAfter modalBg lastBodyElement |> ignore
