module UI

open Sutil
open Sutil.Html
open Sutil.DOM

type UI =
    static member flexColumn (children:SutilElement seq) =
        Html.div (
            [ Attr.styleAppend [ Css.displayFlex; Css.flexDirectionColumn ] ]
            |> Seq.append children)

    static member flexRow (children:SutilElement seq) =
        Html.div (
            [ Attr.styleAppend [ Css.displayFlex; Css.flexDirectionRow ] ]
            |> Seq.append children)
