module Plugin_Template

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

type I_Plugin_Template =
    abstract member show : unit -> unit
    abstract member hide : unit -> unit
    abstract member is_visible : unit -> bool

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
                Html.h1 "Test"
            ]
        ]
    else Html.none

(* Component *)

[<ReactComponent>]
let private Plugin_Template
    (props : {| expose : IRefValue<I_Plugin_Template> |})
    : ReactElement =

    let initial_state = { is_visible = false }

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Plugin_Template with
                member _.show () = dispatch <| Show
                member _.hide () = dispatch <| Hide
                member _.is_visible () : bool = state_ref.current.is_visible
        }
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
