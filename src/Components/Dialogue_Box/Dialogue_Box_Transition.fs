module Dialogue_Box_Transition

// console, window
open Browser.Dom
// IRefValue
open Feliz

open Dialogue_Box_Types
open Fade_Transition
open Fade_Types
open Log
open Units_Of_Measure

(* Debug *)

let private log_module_name = "Dialogue_Box_Transition"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1
let mutable private debug_state_update_counter = 1

let is_running_transition (fade_state : IRefValue<Fade_State<unit>>) (typing_state : IRefValue<Typing_State>) : bool =
    let is_typing =
        match typing_state.current with
        | Empty
        | Idle _ -> false
        | Typing _ -> true
    Fade_Transition.is_running_transition fade_state || is_typing

let cancel_reveal_next_timeout_function
    (configuration : IRefValue<Dialogue_Box_Configuration>)
    (transition_timeout_function_handle : IRefValue<float option>)
    (debug_data : (string * obj) list)
    : unit =

    match transition_timeout_function_handle.current with
    | Some id ->
        do
            window.clearTimeout id
            transition_timeout_function_handle.current <- None
    | None ->
(* If the typing speed is 0, we do not use the transition timeout function. *)
        if configuration.current.typing_speed > 0<milliseconds> then do warn "cancel_reveal_next_timeout_function" false "Tried to cancel reveal next timeout function, but the function was not running." <| debug_data

(* TODO2 We do not use this for now. It is simpler to dispatch the Force_Complete_Typing message. *)
(*
let private force_complete_typing_transition
    (state_1 : Typing_State)
    (dispatch : Typing_Message -> unit)
    (configuration : Dialogue_Box_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    : unit =

    match state_1 with

    | Typing (state_2 : Typing_State_Data) ->
        do
(* If we were typing something, we need to cancel the timeout function that calls reveal_next (). *)
            cancel_transition_timeout_function configuration transition_timeout_function_handle []
            state_2.command_queue_item_id |> Dialogue_Box_Types.Notify_Transition_Complete |> dispatch

(* Ignore the Empty and Idle states. *)
    | _ -> ()
*)

(* This function is called by both Runner_Run.force_complete_transitions (), when the player wants to skip a transition, and (indirectly) by Runner_State.set_state (), when the player wants to load a saved game. In the former case, we notify Runner the transition is complete. In the latter, we do not. *)
let force_complete_transition
    (fade_state : IRefValue<Fade_State<unit>>)
    (typing_state : IRefValue<Typing_State>)
    (fade_dispatch : Fade_Message<unit> -> unit)
    (typing_dispatch : Typing_Message -> unit)
    : unit =

(* TODO2 Both fade state and typing state have command queue IDs.

This component can only have one transition at a time (either visibility or typing). It is unlikely we will ever want to do both types of transition at once, especially as we do not fade the dialogue box in/out, but only show/hide it.

However, this issue might arise again with a different component where we do need to allow multiple transition types at once.
How would we determine which command queue ID (fade or typing) prevails?
We had to deal with this elsewhere in this component - namely, one transition type (either visibility or typing) prevails over the other. See Dialogue_Box_Rendering.view ().

In any case, as with Characters, the answer is likely that the functions for each transition type will receive a notify function that reports not to Runner, but only to this component, which internally tracks which transition types are active and only reports to Runner when all are complete.
*)
    do
(* Again, this could conceivably signal Runner_Queue twice, once for completing a fade transition, once for completing a typing transition.
That should never happen, as any command that involves this component should create only one type of transition.
If it does, we issue a warning.
Runner_Queue.remove_transition will at least issue a warning, because we will try to remove a dialogue_box transition (Runner_Queue only recognizes one type of dialogue box transition) from the same command twice. Runner_Queue.remove_transition might raise an error, if, as a result of removing the dialogue_box transition the first time, the command is also removed before we try to remove the second transition.
*)
        let fade_state_in_transition =
            match fade_state.current with
            | Idle_Visible _
            | Idle_Hidden -> false
            | _ -> true
        let typing_state_in_transition =
            match typing_state.current with
            | Typing _ -> true
            | _ -> false
        if fade_state_in_transition then
            force_complete_fade_transition fade_state fade_dispatch
        if typing_state_in_transition then 
(* Force_Complete_Typing cancels the transition timeout function. *)
            typing_dispatch Force_Complete_Typing
        if fade_state_in_transition && typing_state_in_transition then
            warn "force_complete_transition" false "Both fade state and typing state are in transition." ["fade_state", fade_state.current; "typing_state", typing_state.current]
