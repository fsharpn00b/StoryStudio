module Transition_2

// String.Compare, String.Empty
open System

// console, window
open Browser.Dom
(* 20251001 We are supposed to be using Feliz, not Fable.React. It seems IRefValue is in Fable.React, but can also be provided by Feliz. Using IRefValue only causes an error if we comment out both open Fable.React and open Feliz.
*)
(*
open Fable.React
*)
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Log
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Transition"

let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1

(* Types - public *)

type Transition_Configuration = unit

(* Previously, when this was Fade_Transition, 'T was typically a string that contained a background URL or character sprite URL. Now, 'T represents a higher type such as Visible of string (URL) | Hidden. So we have moved the transition type (for example, fade in/fade out/cross fade) and corresponding state data (visible (url)/hidden) from here to the component (such as Background). That is why Transition_Data contains both old_data and new_data fields, which would not have applied to, for example, fade in (which only needs a new URL) or fade out (which only needs an old URL).
*)
type Transition_Data<'data_type, 'transition_type> = {
    old_data : 'data_type
    new_data : 'data_type
    transition_type : 'transition_type
    transition_time : Transition_Time
    command_queue_item_id : int<runner_queue_item_id>
}

type Transition_State<'data_type, 'transition_type> =
    | Idle of 'data_type
    | In_Transition of Transition_Data<'data_type, 'transition_type>

type Complete_Transition_Func<'data_type> = int<runner_queue_item_id> -> bool -> 'data_type -> unit
type Set_State_Func<'data_type, 'transition_type> = Transition_State<'data_type, 'transition_type> -> unit

(* Functions *)

let is_running_transition (state : IRefValue<Transition_State<'data_type, 'transition_type>>) : bool =
    match state.current with
    | Idle _ -> false
    | In_Transition _ -> true

let force_complete_transition
    (state_1 : IRefValue<Transition_State<'data_type, 'transition_type>>)
    (complete_transition : Complete_Transition_Func<'data_type>)
    : unit =

    do
        match state_1.current with
        | In_Transition state_2 ->
(* Note The onTransitionEnd () handler in the ReactElement is cancelled when the element is replaced by the new Idle state. *)
            complete_transition state_2.command_queue_item_id true state_2.new_data
        | _ -> ()

let begin_transition
    (set_state : Set_State_Func<'data_type, 'transition_type>)
    (notify_transition_complete : int<runner_queue_item_id> -> unit)
    (state : IRefValue<Transition_State<'data_type, 'transition_type>>)
    (new_data : 'data_type)
    (transition_time : Transition_Time)
    (transition_type : 'transition_type)
    (command_queue_item_id : int<runner_queue_item_id>)
    : unit =

    match state.current with

(* If the old and new data are the same, do nothing. *)
    | Idle old_data when old_data = new_data -> notify_transition_complete command_queue_item_id

    | Idle old_data ->
(* If the transition time is 0, just replace the old data. *)
        if transition_time <= 0.0<seconds> then
            set_state <| Idle new_data
            notify_transition_complete command_queue_item_id
        else
            set_state <| In_Transition {
                old_data = old_data
                new_data = new_data
                transition_type = transition_type
                transition_time = transition_time
                command_queue_item_id = command_queue_item_id
            }

(* Otherwise, ignore the call.
Previously, if this method was called during a transition, we would skip the transition. Now we require Runner to call the force_complete_transition () function. That way, we explicitly complete the existing transition and start a new one.*)
    | _ ->
        warn "update_transition" false "Called with unexpected state. Ignoring." ["state", state]
        notify_transition_complete command_queue_item_id

let complete_transition
    (set_state : Set_State_Func<'data_type, 'transition_type>)
    (notify_transition_complete : int<runner_queue_item_id> -> unit)
    (command_queue_item_id : int<runner_queue_item_id>)
    (is_notify_transition_complete : bool)
    (data : 'data_type)
    : unit =

    set_state <| Idle data
    if is_notify_transition_complete then notify_transition_complete command_queue_item_id

(* Component *)

(* TODO1 #transitions Split Fade_Image and the view functions into a separate re-usable component for background, characters, etc.
Could have separate classes - fadeable, moveable, etc.
Could let container components inject React properties.
Could pass in the transition property name and initial/final values. We still need to do React.useState ()/useEffectOnce () in the component, we think.
*)

[<ReactComponent>]
let Transitionable_Image (
    url: string,
    class_name : string,
    additional_properties : IReactProperty list,
    transition_property_name : string,
    transition_property_initial_value : float,
    transition_property_final_value : float,
    transition_time : Transition_Time,
    handle_transition_end : unit -> unit
    ) =

    let transition_property_value, set_transition_property_value = React.useState transition_property_initial_value

    React.useEffectOnce (fun () ->
        window.setTimeout ((fun () -> set_transition_property_value transition_property_final_value), int pre_transition_time) |> ignore
    )

    Html.img [
        prop.key url
        prop.src url
        prop.className class_name
        prop.style [
            style.custom (transition_property_name, transition_property_value)
            style.custom ("transition", $"{transition_property_name} {transition_time}s ease-in-out")
        ]
        prop.onTransitionEnd (fun _ ->
            handle_transition_end ()
        )
        yield! additional_properties
    ]

let get_transitionable_image
    (class_name : string)
    (additional_properties : IReactProperty list)
    (transition_property_name : string)
    (transition_time : Transition_Time)
    (handle_transition_end : unit -> unit)
    (url: string)
    (transition_property_initial_value : float)
    (transition_property_final_value : float)
    : ReactElement =

    Transitionable_Image (url, class_name, additional_properties, transition_property_name, transition_property_initial_value, transition_property_final_value, transition_time, handle_transition_end)
