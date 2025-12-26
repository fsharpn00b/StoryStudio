module Music

// String.Empty
open System

// console
open Browser.Dom
open Elmish
(* 20251001 We are supposed to be using Feliz, not Fable.React. It seems IRefValue is in Fable.React, but can also be provided by Feliz. Using IRefValue only causes an error if we comment out both open Fable.React and open Feliz.
*)
(*
open Fable.React
*)
open Feliz
open Feliz.UseElmish

(* TODO1 Add music.
- Bug. Music stops if we reload page.
- Add fade in/fade out/cross fade.
*)

(* Types *)

type private Message =
    | Stop
    | Play of string

type Music_State =
    | Stopped
    | Playing of string

(* Interfaces *)

type I_Music =
    abstract member stop : unit -> unit
    abstract member play : string -> unit
    abstract member get_state : unit -> Music_State
    abstract member set_state : Music_State -> unit

(* Functions - rendering *)

let private view
    (state : IRefValue<Music_State>)
    : ReactElement =

    match state.current with
    | Playing url ->
        Html.audio [
            prop.src url
            prop.autoPlay true
        ]
    | _ -> Html.none

(* Functions - state *)

let private update
    (message : Message)
    (state : Music_State)
    : Music_State * Cmd<Message> =

    match message with
    | Play url -> Playing url, Cmd.none
    | Stop -> Stopped, Cmd.none

let private set_state
    (old_state : Music_State)
    (new_state : Music_State)
    (dispatch : Message -> unit)
    : unit =

    match new_state with
    | Playing new_url ->
        match old_state with
(* If we are already playing music, and the old music URL and new music URL are the same, then just keep playing. *)
        | Playing old_url when 0 = String.Compare(old_url, new_url) -> ()
        | _ -> dispatch <| Play new_url
    | Stopped -> dispatch <| Stop    

(* Component *)

[<ReactComponent>]
let Music
    (props : {| expose : IRefValue<I_Music> |})
    : ReactElement =

(* State *)

    let state, dispatch = React.useElmish((Stopped, Cmd.none), update, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

    do React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Music with
                member _.stop
                    ()
                    : unit =
                    dispatch Stop
                member _.play
                    (url : string)
                    : unit =
                    dispatch <| Play url
                member _.get_state () : Music_State = state
                member _.set_state (new_state : Music_State) : unit = set_state state new_state dispatch
        }
    )

    view state_ref
