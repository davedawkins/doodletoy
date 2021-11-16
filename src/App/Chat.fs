module Chat

open Sutil
open Sutil.DOM
open type Feliz.length
open AppwriteSdk
open UI
open Types
open Types.Schema

type Model = {
        User : User
        ChatLog : string
        Input : string
    }
    with
        static member Format( m : ChatMessage ) =
            sprintf "From %s: %s" m.user m.message

        member this.Append( m : string ) =
            { this with ChatLog = this.ChatLog + m + "\n" }

        member this.Append( m : ChatMessage ) =
            this.Append( Model.Format(m) )

type Message =
    | SetInput of string
    | SendMessage
    | ServerError of System.Exception
    | ReceivedMessage of ChatMessage

let init user =
    { User = user; ChatLog = ""; Input = "" }, Cmd.none

let update (session : Server.Session) msg model =
    match msg with
    | SetInput s -> { model with Input = s }, Cmd.none

    | SendMessage ->
        let chatMsg = model.Input
        let cmd = Cmd.OfPromise.attempt (fun m -> session.Post(m)) chatMsg ServerError
        { model.Append(chatMsg) with Input = "" }, cmd

    | ServerError x ->
        model.Append(x.Message), Cmd.none

    | ReceivedMessage icm ->
        model.Append(icm), Cmd.none

open Fable.DrawingCanvas
open Fable.DrawingCanvas.Builder

let view (session : Server.Session) model dispatch =

    UI.flexColumn [
        disposeOnUnmount [
            model
            session.Server.Messages.Subscribe( dispatch << ReceivedMessage )
        ]

        Attr.style [ Css.gap (rem 1) ]

        Html.textarea [
            Attr.className "textarea"
            Bind.attr("value", model .> (fun m -> m.ChatLog))
        ]

        Html.input [
            Attr.className "input"
            Attr.typeText
            Attr.placeholder "Message #general"
            Bind.attr("value", model .> (fun m -> m.Input), dispatch << SetInput )
            Ev.onKeyDown(fun ke ->
                if ke.key = "Enter" then
                    ke.preventDefault()
                    dispatch SendMessage
            )
        ]
    ]


let ui (session : Server.Session) =
    let model, dispatch = session.User |> Store.makeElmish init (update session) ignore
    {
        Main = view session model dispatch
        Nav = fragment []
    }