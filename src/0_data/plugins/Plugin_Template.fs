module Plugin_Template

// Environment
open System

// console, window
open Browser.Dom
// Cmd
open Elmish
// ? operator, importSideEffects
open Fable.Core.JsInterop
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz
// useElmish
open Feliz.UseElmish
// Decode, Encode
open Thoth.Json

(* Note This file is in the project only so Fable will compile it to .js, which we can then import to dynamically load the component. *)

(* Import CSS. *)
importSideEffects "./Plugin_Template.css"

(* Types *)

type private Plugin_Template_State = {
    is_visible : bool
}

type private Plugin_Template_Message =
    | Show
    | Hide

(* Interfaces *)

(* Main functions - state *)

let private update
    (message : Plugin_Template_Message)
    (state : Plugin_Template_State)
    : Plugin_Template_State * Cmd<Plugin_Template_Message> =

    match message with

    | Show ->
        {
            state with
                is_visible = true
        }, Cmd.none

    | Hide -> { state with is_visible = false }, Cmd.none

(* Main functions - rendering *)

let private view 
    (state : IRefValue<Plugin_Template_State>)
    (dispatch : Plugin_Template_Message -> unit)
    : ReactElement =

    if state.current.is_visible then
        Html.div [
            prop.id "plugin_template"
            prop.children [
                Html.h1 "Plugin test"
            ]
        ]
    else Html.none

(* Component *)

[<ReactComponent>]
let private Plugin_Template
    (props : {| expose : IRefValue<obj> |})
    : ReactElement =

    let initial_state = { is_visible = false }

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

    React.useImperativeHandle(props.expose, fun () ->
        createObj [
(* JS-facing API. *)
            "show" ==> fun () -> dispatch <| Show
            "hide" ==> fun () -> dispatch <| Hide
            "is_visible" ==> fun () -> state_ref.current.is_visible
(* Framework-facing API. *)
            "get_state" ==> fun () -> Encode.Auto.toString (0, state_ref.current)
            "set_state" ==> fun (state_json : string) ->
                match Decode.Auto.fromString<Plugin_Template_State> state_json with
                | Ok state ->
                    match state.is_visible with
                    | true -> dispatch <| Show
                    | false -> dispatch <| Hide
                | Error message ->
                    $"Plugin_Template: Failed to deserialize state. Message: {message}{Environment.NewLine}State JSON:{Environment.NewLine}{state_json}" |> invalidOp
        ]
    )

(* Render *)

    view state_ref dispatch

window?Plugins?Plugin_Template <- Plugin_Template

(* To call these methods in a script: *)
(*
js
window.Interfaces.<plugin_name>.current.show ()
endjs
*)
(* For example: *)
(*
js
window.Interfaces.Plugin_Template.current.show ()
endjs
*)
(* The plugin name is configured in 0_data/plugins.txt. *)
