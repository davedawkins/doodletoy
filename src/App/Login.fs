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

type Model = {
    Email : string
    Password : string
    Status : string
}

type Message =
    | SetError of System.Exception
    | SetStatus of string

let init() =
    { Email = ""; Password = ""; Status = "" }, Cmd.none

let update server msg model =
    match msg with
    | SetError x ->
        { model with Status = x.Message}, Cmd.none
    | SetStatus s ->
        { model with Status = s}, Cmd.none

let view (server : Server.Server) =
    let model, dispatch = () |> Store.makeElmish init (update server) ignore

    UI.flexColumn [
        Attr.style [ Css.gap (rem 1) ]

        Html.input [
            Attr.className "input"
            Attr.typeText
            Attr.placeholder "Email"
        ]

        Html.input [
            Attr.className "input"
            Attr.typePassword
            Attr.placeholder "Password"
        ]

        UI.flexRow [
            Attr.style [ Css.gap (rem 1) ]
            Html.button [
                Attr.className "button is-primary"
                text "Sign In"
            ]
            Html.button [
                Attr.className "button is-primary"
                text "Sign In with Google"
                Ev.onClick (fun e -> server.SignInWithGoogle())
            ]
        ]

        Html.div [
            class' "status"
            Html.text (model .> (fun m -> m.Status))
        ]
    ]
