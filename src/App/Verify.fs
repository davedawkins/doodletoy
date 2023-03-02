module Verify

open System
open Sutil
open Server
open Types
open type Feliz.length

type Model = {
    Status : string
}

type Message =
    | SetStatus of string
    | SendEmail
    | Error of Exception

let init (sendEmail,server) =
    { Status = "" }, if sendEmail then Cmd.ofMsg SendEmail else Cmd.none

let update (server :Server) msg model =
    match msg with
    | SetStatus s -> { model with Status = s }, Cmd.none
    | SendEmail ->
        model, Cmd.OfPromise.either (server.SendVerificationEmail) () (fun _ -> SetStatus "Email sent - please allow at least 20 minutes") Error
    | Error x ->
        { model with Status = x.Message }, Cmd.none

let view (sendEmail : bool) (server : Server) =
    let model, dispatch = (sendEmail,server) |> Store.makeElmish init (update server) ignore

    Html.div [
        CoreElements.disposeOnUnmount [ model ]

        Attr.style [
            Css.textAlignCenter
        ]

        Html.div(model |> Store.map (fun m -> m.Status))

        Html.div [
            text "Awaiting confirmation of email address - please check your junk email folder!"
        ]

        Html.div [
            Html.a [
                Attr.href "#"
                text "Resend verification email"
                Ev.onClick (fun e -> e.preventDefault(); dispatch SendEmail)
            ]
        ]

    ]
