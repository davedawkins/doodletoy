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

let view (server : Server) (dispatchExternal) =
    let model, dispatch = () |> Store.makeElmish init (update server) ignore

    UI.flexColumn [
        Attr.style [ Css.gap (rem 1) ]

        Html.input [
            Attr.className "input"
            Attr.typeText
            Attr.placeholder "Email"
            Bind.attr("value", model .> (fun m -> m.Email), dispatch<<SetEmail)
        ]

        Html.input [
            Attr.className "input"
            Attr.typePassword
            Attr.placeholder "Password"
            Bind.attr("value", model .> (fun m -> m.Password), dispatch<<SetPassword)
        ]

        UI.flexRow [
            Attr.style [ Css.gap (rem 1) ]
            Html.button [
                Attr.className "button is-primary"
                text "Sign In"
                Ev.onClick (fun e -> dispatch SignIn)
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
