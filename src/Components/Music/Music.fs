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

(* Types *)

type private Message =
    | Stop
    | Play of string

type private State =
    | Stopped
    | Playing of string

(* Interfaces *)

type I_Music =
    abstract member stop : unit -> unit
    abstract member play : string -> unit

(* Functions - rendering *)

let private view
    (state : IRefValue<State>)
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
    (state_1 : State)
    : State * Cmd<Message> =

    match message with

    | Play url -> Playing url, Cmd.none

    | Stop -> Stopped, Cmd.none

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
        }
    )

    view state_ref
