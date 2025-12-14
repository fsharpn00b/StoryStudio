module Fade_Transition

// Environment.NewLine
open System

// console, window
open Browser.Dom
open Elmish
open Feliz
(* We do not use this for now. *)
//open Feliz.UseElmish

open Fade_Types
open Log
open Units_Of_Measure

(* Debug *)

let private log_module_name = "Fade_Transition"

let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1

let mutable private debug_state_update_counter = 1

(* Helpers*)

let cancel_transition_timeout_function
    (configuration : Fade_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    (debug_data : (string * obj) list)
    : unit =
    match transition_timeout_function_handle.current with
    | Some id ->
        do
            window.clearTimeout id
            transition_timeout_function_handle.current <- None
    | None -> do warn "cancel_transition_timeout_function" false "Tried to cancel transition timeout function, but the function was not running." <| debug_data

let is_running_transition (state : IRefValue<Fade_State<'a>>) : bool =
    match state.current with
    | Idle_Hidden
    | Idle_Visible _ -> false
    | Cross_Fade_Pre_Transition _
    | Cross_Fade_Transition _
    | Fade_In_Pre_Transition _
    | Fade_In_Transition _
    | Fade_Out_Pre_Transition _
    | Fade_Out_Transition _ -> true

(* Main functions *)

let update_begin_transition
    (configuration : Fade_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    (state : Fade_State<'T>)
    : Fade_State<'T> * Cmd<Fade_Message<'T>> =

    match state with

    | Fade_In_Pre_Transition transition_data ->
        #if debug
        do debug "update_begin_transition" "Starting Fade_In_Transition -> Idle_Visible timeout." ["state", state; "transition_data", transition_data]
        #endif
        Fade_In_Transition transition_data,
            Cmd.ofEffect (fun dispatch ->
                do transition_timeout_function_handle.current <- Some <| window.setTimeout((fun () ->
                    #if debug
                    do debug "Fade_In_Transition -> Idle_Visible timeout" String.Empty ["state", state; "transition_data", transition_data]
                    #endif
                    dispatch Complete_Fade_Transition
                ), int (transition_data.transition_time * 1000.0))
            )

    | Fade_Out_Pre_Transition transition_data ->
        #if debug
        do debug "update_begin_transition" "Starting Fade_Out_Transition -> Idle_Hidden timeout." ["state", state; "transition_data", transition_data]
        #endif
        Fade_Out_Transition transition_data,
(* TODO2 How does this know the signature of dispatch? Does it infer it from the function return type?
Fade_State<'T> * Cmd<Fade_Msg<'T>>
*)
            Cmd.ofEffect (fun dispatch ->
                do transition_timeout_function_handle.current <- Some <| window.setTimeout((fun () ->
                    #if debug
                    do debug "Fade_Out_Transition -> Idle_Hidden timeout" String.Empty ["state", state; "transition_data", transition_data]
                    #endif
                    dispatch Complete_Fade_Transition
                ), int (transition_data.transition_time * 1000.0))
            )

    | Cross_Fade_Pre_Transition transition_data ->
        #if debug
        do debug "update_begin_transition" "Starting Cross_Fade_Transition -> Idle_Visible timeout." ["state", state; "transition_data", transition_data]
        #endif
        Cross_Fade_Transition transition_data,
            Cmd.ofEffect (fun dispatch ->
                do transition_timeout_function_handle.current <- Some <| window.setTimeout((fun () ->
                    #if debug
                    do debug "Cross_Fade_Transition -> Idle_Visible timeout." String.Empty ["state", state; "transition_data", transition_data]
                    #endif
                    dispatch Complete_Fade_Transition
                ), int (transition_data.transition_time * 1000.0))
            )

    | _ ->
        do warn "update_begin_transition" false "Called with unexpected state. Ignoring." ["state", state]
        state, Cmd.none

(* This does not cancel the transition timeout because the call to it is dispatched by the timeout itself. *)
let update_complete_transition
    (configuration : Fade_Configuration)
    (state : Fade_State<'T>)
    : Fade_State<'T> * Cmd<Fade_Message<'T>> =

    match state with

    | Fade_In_Transition transition_data ->
        #if debug
        do debug "update_complete_transition" "Completing Fade_In_Transition." ["state", state; "transition_data", transition_data]
        #endif
(* Notify the UI component container (Runner) that the transition is complete, so it can run the next command if needed.
We must do this after a delay. The state should be updated before the command runs, but it seems it is not, due to the asynchronous nature of React.
If we do not delay the dispatch of the message, we enter an infinite loop.
1 We dispatch message Notify_Transition_Complete.
2 Notify_Transition_Complete is handled by Fade_Visibility.update ().
3 That calls the notify_transition_complete () closure that was passed to the constructor of the component that uses Fade.
4 That calls Runner_Run.get_next_command ().
5 That calls Runner_Run.force_complete_transitions ().
6 That calls <UI component>.is_running_transition ().
7 That returns true because the state is still Fade_In_Transition/Fade_Out_Transition/Cross_Fade_Transition.
8 Runner_Run.force_complete_transitions () calls <UI component>.force_complete_transition ().
9 That calls Fade_Transition.force_complete_fade_transition () with notify_transition_complete = true.
10 That dispatches Show or Hide, with notify_transition_complete = true.
11 That is handled by Fade_Visibility.update_show ()/update_hide ().
12 That dispatches message Notify_Transition_Complete (step 1).

For now, any time we change the state, we dispatch message Notify_Transition_Complete with delay set to true.

20251116 The delay is no longer needed due to the restructuring of Runner so it can wait for multiple transitions to complete before it runs the next command.
20251118 The delay is still needed. We now always delay in Runner_Transition.get_notify_transition_complete (). See notes in Menu.update () and Runner_Transition.get_notify_transition_complete ().
*)
        Idle_Visible transition_data.new_data, transition_data.command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg

    | Fade_Out_Transition transition_data ->
        #if debug
        do debug "update_complete_transition" "Completing Fade_Out_Transition." ["state", state; "transition_data", transition_data]
        #endif
        Idle_Hidden, transition_data.command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg

    | Cross_Fade_Transition transition_data ->
        #if debug
        do debug "update_complete_transition" "Completing Cross_Fade_Transition." ["state", state; "transition_data", transition_data]
        #endif
        Idle_Visible transition_data.new_data, transition_data.command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg

(* Complete_Fade_Transition is only dispatched by transition timeout functions, so this means a transition completed on its own rather than being forced to complete by the player. force_complete_fade_transition (), which forces the completion of a transition, does not dispatch Complete_Fade_Transition. *)
    | _ ->
        do warn "update_complete_transition" false "Called with unexpected state. Ignoring." ["state", state]
        state, Cmd.none

(* Note This function forces the completion of a transition. *)
let force_complete_fade_transition
    (state : IRefValue<Fade_State<'a>>)
    (dispatch : Fade_Message<'a> -> unit)
    : unit =

    do
        match state.current with
        | Cross_Fade_Pre_Transition state_2
        | Cross_Fade_Transition state_2 ->
            dispatch <| Show {
                data = state_2.new_data
                is_notify_transition_complete = true
                command_queue_item_id = Some state_2.command_queue_item_id
            }
        | Fade_In_Pre_Transition state_2
        | Fade_In_Transition state_2 ->
            dispatch <| Show {
                data = state_2.new_data
                is_notify_transition_complete = true
                command_queue_item_id = Some state_2.command_queue_item_id
            }
        | Fade_Out_Pre_Transition state_2
        | Fade_Out_Transition state_2 ->
            dispatch <| Hide {
                is_notify_transition_complete = true
                command_queue_item_id = Some state_2.command_queue_item_id
            }

        | _ -> ()
