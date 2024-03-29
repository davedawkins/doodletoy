module Profile

open Sutil
open Types
open Server
open Sutil.Styling
open AppwriteSdk
open UI
open type Feliz.length
open type Feliz.borderStyle
open Sutil.CoreElements

type Model = {
    Doodles : DoodleView list
    ErrorMessage : string option
}

type Message =
    | GetDoodles
    | GotDoodles of DoodleView[]
    | Delete of Schema.Doodle
    | Deleted
    | Error of System.Exception
    | SetErrorMessage of string option

let always a _ = a

let init() =
    {
        Doodles = []
        ErrorMessage = None
    }, Cmd.ofMsg GetDoodles

let update (session : DoodleSession) msg model =
    match msg with
    | Delete d ->
        model, Cmd.OfPromise.either (session.Server.DeleteDoodle) (d._id) (always Deleted) Error
    | Deleted->
        model, Cmd.ofMsg GetDoodles
    | Error x ->
        model, Cmd.ofMsg (SetErrorMessage (Some x.Message))
    | SetErrorMessage s ->
        { model with ErrorMessage = s}, Cmd.none
    | GetDoodles ->
        model,
        Cmd.OfPromise.result (session.UserDoodles() |> Promise.map GotDoodles)
    | GotDoodles doodles ->
        { model with Doodles = doodles |> List.ofArray }, Cmd.none

let style = [
    rule ".profile-userinfo" [
        Css.marginBottom (rem 1)
    ]

    rule ".profile-userinfo > div" [
        Css.displayFlex
    ]

    rule ".profile-userinfo label" [
        Css.fontWeightBold
        Css.displayInlineBlock
        Css.minWidth (rem 4)
    ]


    rule ".name" [
        Css.cursorPointer
    ]
    rule ".name:hover" [
        Css.fontWeightBold
    ]

    rule ".doodle-grid" [
        Css.displayGrid
        Css.gridTemplateColumns( [fr 11; fr 1] )
        Css.borderTop(px 1, solid, "#dddddd")
    ]

    CssMedia.minWidth(UI.BreakPoint, [
        rule ".doodle-grid" [
            Css.gridTemplateColumns( [fr 3; fr 8; fr 1; fr 3; fr 3; fr 1] )
        ]
    ])

    rule ".doodle-grid>*" [
//        Css.borderBottom(px 1, solid, "#dddddd")
        Css.paddingLeft (rem 0.2)
        Css.paddingRight (rem 0.2)
    ]

    rule ".header" [
        Css.fontWeightBold
        Css.borderBottom(px 1, solid, "#dddddd")
    ]

    rule "h4" [
        Css.fontSize (rem 1.5)
        Css.marginBottom (rem 1)
    ]

    rule ".fa-trash-alt" [
        Css.color "hsla(257.6, 56.9%, 20%, 0.20)"
        Css.cursorPointer
    ]

    rule ".fa-trash-alt:hover" [
        Css.color "hsla(257.6, 56.9%, 20%, 0.30)"
    ]

    rule ".fa-trash-alt:active" [
        Css.color "hsla(257.6, 56.9%, 20%, 0.40)"
    ]

    rule ".error" [
        Css.color "red"
    ]

    rule ".if-wide" [
        Css.displayNone
    ]

    CssMedia.minWidth(UI.BreakPoint,[
        rule ".if-wide" [
            Css.displayInheritFromParent
        ]
    ])
]

let formatDT formatString (date : System.DateTime) = Date.Format.localFormat Date.Local.englishUK formatString date
let asDateTime n = System.DateTime(int64(n) * System.TimeSpan.TicksPerSecond)

let viewRecord (server : Server) (view : DoodleView ) dispatch =
    let span content (media : string) = Html.span  [ Attr.className (media); text content]
    let t = view.Doodle

    fragment [
        Html.span [
            Attr.className "name"
            text t.name
            Ev.onClick (fun _ -> Browser.Dom.window.location.href <- "#edit?d=" + t._id)
        ]
        span t.description "if-wide"
        span (match t.isPrivate with true -> "Yes"|false -> "No")  "if-wide"
        span (asDateTime(t.createdOn) |> formatDT "yyyy-MM-dd hh:mm:ss")  "if-wide"
        span (asDateTime(t.modifiedOn) |> formatDT "yyyy-MM-dd hh:mm:ss")  "if-wide"
        Html.span [
            Html.i [ Attr.className "fas fa-trash-alt" ]
            Ev.onClick (fun _ ->
                {
                    UI.ModalOptions.Create()
                        with
                            Content  = fun close -> Html.div (sprintf "Delete '%s' ?" t.name)
                            Buttons  = [
                                ("OK", fun c -> dispatch (Delete t); c())
                                ("Cancel", fun c -> c()) ]
                } |> UI.modal
            )
        ]
    ]

let view (session : DoodleSession) server =
    let model , dispatch = () |> Store.makeElmish init (update session) ignore

    let header name media = Html.span  [ Attr.className ("header " + media); text name]
    Html.div [
        Attr.className "profile-container"
        Html.div [
            Attr.className "profile-userinfo"

            Html.div [
                Html.label [  text "Name:"  ]
                Html.span (text (session.User.name))
            ]
            Html.div [
                Html.label [  text "Email:"  ]
                Html.span (text (session.User.email))
            ]
        ]
        Bind.el( model |> Store.map (fun m -> m.ErrorMessage), fun optMsg ->
            match optMsg with
            | None -> Html.div []
            | Some m -> UI.divc "error" [ text m ]
        )
        Html.div [
            Html.h4 "Your Doodles"
        ]
        UI.divc "doodle-grid" [
            fragment [
                header "Name" ""
                header "Description" "if-wide"
                header "Private" "if-wide"
                header "Created" "if-wide"
                header "Modified" "if-wide"
                header "" ""
            ]
            Bind.el(model |> Store.map (fun m -> m.Doodles), fun doodles ->
                fragment [
                    for d in doodles do
                        viewRecord server d dispatch
                ]
            )
        ]
    ] |> withStyle style