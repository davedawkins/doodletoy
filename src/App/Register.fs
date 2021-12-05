module Register

open Sutil
open Sutil.Attr
open Feliz
open type Feliz.length
open Sutil.Styling
open UI
open Types
open Server

type Model = {
    Email : string
    Password : string
    Name : string
    Verification : string
    Status : string
    //RegistrationCompleted : bool
}

type Message =
    | SetError of System.Exception
    | SetStatus of string
    | SetEmail of string
    | SetName of string
    | SetPassword of string
    | SetVerification of string
    | Register
    | Registered

let init() =
    { Email = ""; Password = ""; Verification = ""; Name = ""; Status = "" }, Cmd.none

[<AutoOpen>]
module Validation =
    let verifyEmail m =
        m.Email.IndexOf('@') >= 1

    let verifyName m =
        m.Name <> ""

    let verifyPassword m =
        m.Password.Length >= 8

    let verifyVerify m =
        verifyPassword m && m.Password = m.Verification

    let verify m =
        if not (verifyEmail m) then failwith "Valid email is required"
        if not (verifyPassword m) then failwith "Password not long enough (>= 8 characters)"
        if not (verifyName m) then failwith "Name is required"
        if not (verifyVerify m) then failwith "Password verification doesn't match"

let update (server : Server) msg model =
    Fable.Core.JS.console.log($"{msg}")
    match msg with
    | SetError x ->
        { model with Status = x.Message}, Cmd.none
    | SetStatus s ->
        { model with Status = s}, Cmd.none
    | SetEmail email ->
        { model with Email = email }, Cmd.none
    | SetName name ->
        { model with Name = name }, Cmd.none
    | SetPassword pwd ->
        { model with Password = pwd }, Cmd.none
    | SetVerification pwd ->
        { model with Verification = pwd }, Cmd.none
    | Registered ->
        server.Dispatch RegisteredNewAccount
        model, Cmd.none
    | Register ->
        try
            verify model
            model, Cmd.OfPromise.either (server.Register) (model.Email, model.Password, model.Name) (fun _ -> Registered) SetError
        with
        | x -> model, Cmd.ofMsg (SetError x)

let style = [
    rule ".register" [
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

    rule ".register>*" [
        Css.width (percent 90)
    ]

    rule ".register>span" [
        Css.width (auto)
    ]
    rule ".buttons" [
        Css.displayFlex
        Css.flexDirectionColumn
        Css.alignItemsCenter
        Css.width (rem 15)
    ]

    rule ".register button" [
        Css.width (rem 15)
        Css.displayInlineFlex
        Css.justifyContentSpaceAround
        Css.alignItemsCenter
    ]

    rule ".status" [
        Css.color "red"
    ]
]

let successClass status = if status then "success" else "error"

let view (server : Server) =
    let model, dispatch = () |> Store.makeElmish init (update server) ignore

    UI.divc "register" [

        Html.h2 "Register New Account"

        UI.divc "label-input" [
            Html.label [ text "Name" ]
            Html.input [
                Attr.name "name"
                Attr.autoComplete "nope"
                Bind.className( model .> (verifyName>>successClass) )
                Attr.typeText
                Attr.placeholder "Name"
                Bind.attr("value", model .> (fun m -> m.Name), dispatch<<SetName)
            ]
        ]

        UI.divc "label-input" [
            Html.label [ text "Email" ]
            Html.input [
                Attr.name "email"
                Attr.autoComplete "nope"
                Bind.className( model .> (verifyEmail>>successClass) )
                Attr.typeText
                Attr.placeholder "Email"
                Bind.attr("value", model .> (fun m -> m.Email), dispatch<<SetEmail)
            ]
        ]

        UI.divc "label-input" [
            Html.label [ text "Password" ]
            Html.input [
                Attr.name "password"
                Attr.autoComplete "new-password"
                Bind.className( model .> (verifyPassword>>successClass) )
                Attr.typePassword
                Attr.placeholder "Password"
                Bind.attr("value", model .> (fun m -> m.Password), dispatch<<SetPassword)
            ]
        ]

        UI.divc "label-input" [
            Html.label [ text "Verify Password" ]
            Html.input [
                Attr.name "verify"
                Attr.autoComplete "new-password"
                Bind.className( model .> (verifyVerify>>successClass) )
                Attr.typePassword
                Attr.placeholder "Verify Password"
                Bind.attr("value", model .> (fun m -> m.Verification), dispatch<<SetVerification)
            ]
        ]

        Html.div [
            class' "status"
            Html.text (model .> (fun m -> m.Status))
        ]

        Html.button [
            text "Register"
            Ev.onClick (fun e -> dispatch Register)
        ]

    ] |> withStyle style
