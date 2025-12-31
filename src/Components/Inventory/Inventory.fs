module Inventory

open System

// Cmd
open Elmish
// exportDefault
open Fable.Core.JsInterop
// IRefValue
open Feliz
// useElmish
open Feliz.UseElmish

(* Note This file is in the project only so Fable will compile it to .js, which we can then import to dynamically load the component. *)

// TODO1 #dynamic_load Add separate css file.

(* Types *)

type private Inventory_State = {
    is_visible : bool
}

type private Inventory_Message =
    | Show
    | Hide

(* Interfaces *)

type I_Inventory =
    abstract member show : unit -> unit
    abstract member hide : unit -> unit
    abstract member is_visible : unit -> bool

(* Main functions - state *)

let private update
    (message : Inventory_Message)
    (state : Inventory_State)
    : Inventory_State * Cmd<Inventory_Message> =

    match message with

    | Show ->
        {
            state with
                is_visible = true
        }, Cmd.none

    | Hide -> { state with is_visible = false }, Cmd.none

(* Main functions - rendering *)

let private view 
    (state : IRefValue<Inventory_State>)
    (dispatch : Inventory_Message -> unit)
    : ReactElement =

    if state.current.is_visible then
        Html.div [
            Html.h1 [
                prop.style [
                    style.top 100
                    style.left 100
                    style.custom ("position", "absolute")
                    style.zIndex 10
                ]
                prop.text $"Test"
            ]
        ]
    else Html.none

(* Component *)

[<ReactComponent>]
let private Inventory
    (props : {| expose : IRefValue<I_Inventory> |})
    : ReactElement =

    let initial_state = { is_visible = false }

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Inventory with
                member _.show () = dispatch <| Show
                member _.hide () = dispatch <| Hide
                member _.is_visible (): bool = state_ref.current.is_visible
        }
    )

(* Render *)

    view state_ref dispatch

Browser.Dom.window?Plugins?Inventory <- Inventory
