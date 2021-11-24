module Login

open Sutil
open Sutil.DOM
open Sutil.Attr
open Sutil.Html
open Feliz
open type Feliz.length
open Fable.Core
open Fable.Core.JsInterop
open AppwriteSdk
open Sutil.Styling
open UI
open Server

type Model = {
    Email : string
    Password : string
    Status : string
}

type Message =
    | SetError of System.Exception
    | SetStatus of string
    | SetEmail of string
    | SetPassword of string
    | SignIn

let init() =
    { Email = ""; Password = ""; Status = "" }, Cmd.none

let update (server : Server) msg model =
    match msg with
    | SetError x ->
        { model with Status = x.Message}, Cmd.none
    | SetStatus s ->
        { model with Status = s}, Cmd.none
    | SetEmail email ->
        { model with Email = email }, Cmd.none
    | SetPassword pwd ->
        { model with Password = pwd }, Cmd.none
    | SignIn ->
        server.SignIn(model.Email, model.Password)
        model, Cmd.none

let style = [
    rule ".login" [
        Css.padding (rem 1)
        Css.gap (rem 1)
        Css.displayFlex
        Css.flexDirectionColumn
        Css.alignItemsCenter
        Css.maxWidth (px 500)
        Css.margin(px 0, auto)

        Css.border(px 1, borderStyle.solid, "#dddddd")
        Css.borderRadius(px 10)
    ]

    rule ".label-input" [
        Css.displayFlex
        Css.flexDirectionColumn
    ]

    rule ".login>*" [
        Css.width (percent 90)
    ]

    rule ".login>span" [
        Css.width (auto)
    ]
    rule ".buttons" [
        Css.displayFlex
        Css.flexDirectionColumn
        Css.alignItemsCenter
        Css.width (rem 15)
    ]

    rule ".login button" [
        Css.width (rem 15)
        Css.displayInlineFlex
        Css.justifyContentSpaceAround
        Css.alignItemsCenter
    ]
]

let view (server : Server) =
    let model, dispatch = () |> Store.makeElmish init (update server) ignore

    UI.divc "login" [

        Html.h2 "Sign In"

        UI.divc "label-input" [
            Html.label [ text "Email" ]
            Html.input [
                Attr.typeText
                Attr.placeholder "Email"
                Bind.attr("value", model .> (fun m -> m.Email), dispatch<<SetEmail)
            ]
        ]

        UI.divc "label-input" [
            Html.label [ text "Password" ]
            Html.input [
                Attr.typePassword
                Attr.placeholder "Password"
                Bind.attr("value", model .> (fun m -> m.Password), dispatch<<SetPassword)
            ]
        ]

        Html.span " "

        Html.button [
            text "Sign In"
            Ev.onClick (fun e -> dispatch SignIn)
        ]

        Html.span "or"
        Html.button [
            Html.span "Continue with Google "
            Html.i [ Attr.style [ Css.color "rgb(80,135,236)"]; Attr.className "fa fa-google" ]
            Ev.onClick (fun e -> server.SignInWith("google"))
        ]

        Html.button [
            Html.span "Continue with Github "
            Html.i [ Attr.style [ Css.color "black"]; Attr.className "fa fa-github" ]
            Ev.onClick (fun e -> server.SignInWith("github"))
        ]

        Html.button [
            Html.span "Continue with Discord "
            Html.i [ Attr.style [ Css.color "rgb(87,106,234)"]; Attr.className "fab fa-discord" ]
            Ev.onClick (fun e -> server.SignInWith("discord"))
        ]

        Html.div [
            class' "status"
            Html.text (model .> (fun m -> m.Status))
        ]
    ] |> withStyle style
