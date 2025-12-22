module Fade_Visibility

// String.Empty
open System

// console, window
open Browser.Dom
open Elmish
open Feliz
(* We do not use this for now. *)
//open Feliz.UseElmish

open Fade_Transition
open Fade_Types
open Log
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Fade_Visibility"

let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1

let mutable private debug_state_update_counter = 1

(* Fade out/fade in/cross fade *)

let private update_fade_out<'T when 'T : equality>
    (configuration : Fade_Configuration)
    (state : Fade_State<'T>)
    (fade_out_data : Fade_Out_Message_Data)
    (transition_timeout_function_handle : IRefValue<float option>)
    : Fade_State<'T> * Cmd<Fade_Message<'T>> =

    let debug_data : (string * obj) list = ["state", state; "transition_timeout_function_handle", transition_timeout_function_handle.current]

    let command = fade_out_data.command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg 

    match state with

(* If the image is already hidden, do nothing. *)
    | Idle_Hidden ->
(* It seems we need a delay after handling every message, even when the state does not change. Otherwise, we get behavior such as
- Commands that should automatically run the next command afterward, but do not.
- Commands after which we cannot run the next command even manually.
- Infinite loops, though we are not sure why. See also the notes for update_complete_transition (), but the infinite loops we encountered did not even involve changing the state.

TODO2 Maybe Notify_Transition_Complete should always delay. We cannot think of a case where we can safely set delay to false.
*)
        Idle_Hidden, command

    | Idle_Visible old_data ->
(* If the transition time is 0, just hide the old data. *)
        if fade_out_data.transition_time <= 0.0<seconds> then
            state, command

        else
(* Trigger the first render, to set up the old data before fading out. *)
            #if debug
            do debug "update_fade_out" "Starting Fade_Out_Pre_Transition -> Fade_Out_Transition timeout." <| debug_data @ ["old_data", old_data]
            #endif
            Fade_Out_Pre_Transition {
                old_data = old_data
                transition_time = fade_out_data.transition_time
                command_queue_item_id = fade_out_data.command_queue_item_id
            },
            Cmd.ofEffect (fun dispatch ->
                window.setTimeout((fun () ->
                    #if debug
                    do debug "Fade_Out_Pre_Transition -> Fade_Out_Transition timeout" String.Empty <| debug_data @ ["old_data", old_data]
                    #endif
                    dispatch Begin_Fade_Transition
                ), int pre_transition_time) |> ignore
            )

(* Otherwise, ignore the call.
Previously, if this method was called during a transition, we would skip the transition. Now we require Runner to call the force_complete_transition () function. That way, we explicitly complete the existing transition and start a new one.*)
    | _ ->
        do warn "update_fade_out" false "Called with unexpected state. Ignoring." debug_data
        state, command

let rec update_fade_in<'T when 'T : equality>
    (configuration : Fade_Configuration)
    (state : Fade_State<'T>)
    (fade_in_data : Fade_In_Message_Data<'T>)
    (transition_timeout_function_handle : IRefValue<float option>)
    : Fade_State<'T> * Cmd<Fade_Message<'T>> =

    let debug_data : (string * obj) list = ["state", state; "fade_in_data", fade_in_data; "transition_timeout_function_handle", transition_timeout_function_handle.current]

    let command = fade_in_data.command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg

    match state with

    | Idle_Hidden ->
(* If the transition time is 0, just show the new data. *)
        if fade_in_data.transition_time <= 0.0<seconds> then
            Idle_Visible fade_in_data.new_data, command
        else
(* Trigger the first render, to set up the new data before fading in. *)
            #if debug
            do debug "update_fade_in" "Starting Fade_In_Pre_Transition -> Fade_In_Transition timeout." debug_data
            #endif
            Fade_In_Pre_Transition { new_data = fade_in_data.new_data; transition_time = fade_in_data.transition_time; command_queue_item_id = fade_in_data.command_queue_item_id },
            Cmd.ofEffect (fun dispatch ->
                window.setTimeout((fun () ->
                    #if debug
                    do debug "Fade_In_Pre_Transition -> Fade_In_Transition timeout" String.Empty debug_data
                    #endif
                    dispatch Begin_Fade_Transition
                ), int pre_transition_time) |> ignore
            )

(* If the same data is already visible, do nothing. *)
    | Idle_Visible old_data when old_data = fade_in_data.new_data ->
        state, command

(* If data is already visible, but not the same as the new data, cross-fade instead. *)
    | Idle_Visible _ ->
        update_cross_fade configuration state { new_data = fade_in_data.new_data; transition_time = fade_in_data.transition_time; command_queue_item_id = fade_in_data.command_queue_item_id } transition_timeout_function_handle

(* Otherwise, ignore the call.
Previously, if this method was called during a transition, we would skip the transition. Now we require Runner to call the force_complete_transition () function. That way, we explicitly complete the existing transition and start a new one.*)
    | _ ->
        do warn "update_fade_in" false "Called with unexpected state. Ignoring." debug_data
        state, command

and update_cross_fade<'T when 'T : equality>
    (configuration : Fade_Configuration)
    (state : Fade_State<'T>)
    (cross_fade_data : Cross_Fade_Message_Data<'T>)
    (transition_timeout_function_handle : IRefValue<float option>)
    : Fade_State<'T> * Cmd<Fade_Message<'T>> =

    let debug_data : (string * obj) list = ["state", state; "cross_fade_data", cross_fade_data; "transition_timeout_function_handle", transition_timeout_function_handle.current]

    let command = cross_fade_data.command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg 

    match state with

(* If the image is hidden, fade in instead. *)
    | Idle_Hidden ->
        update_fade_in configuration state { new_data = cross_fade_data.new_data; transition_time = cross_fade_data.transition_time; command_queue_item_id = cross_fade_data.command_queue_item_id } transition_timeout_function_handle
// TODO2 Alternately, we could dispatch Fade_In message.
(*
        state, Cmd.ofEffect (fun dispatch -> dispatch <| Fade_In { Fade_In_Message_Data.new_data = cross_fade_data.new_data; transition_time = cross_fade_data.transition_time })
*)

(* If the data is already visible, do nothing. *)
    | Idle_Visible old_data when old_data = cross_fade_data.new_data ->
        state, command

    | Idle_Visible old_data ->
(* If the transition time is 0, just show the new data. *)
        if cross_fade_data.transition_time <= 0.0<seconds> then
            Idle_Visible cross_fade_data.new_data, command
        else
(* Trigger the first render, to set up the old and new data before transitioning from old to new. *)
            #if debug
            do debug "update_cross_fade" "Starting Cross_Fade_Pre_Transition -> Cross_Fade_Transition timeout." <| debug_data @ ["old_data", old_data :> obj]
            #endif
            Cross_Fade_Pre_Transition { old_data = old_data; new_data = cross_fade_data.new_data; transition_time = cross_fade_data.transition_time; command_queue_item_id = cross_fade_data.command_queue_item_id },
            Cmd.ofEffect (fun dispatch ->
                window.setTimeout((fun () ->
                    #if debug
                    do debug "Cross_Fade_Pre_Transition -> Cross_Fade_Transition timeout" String.Empty <| debug_data @ ["old_data", old_data :> obj]
                    #endif
                    dispatch Begin_Fade_Transition
                ), int pre_transition_time) |> ignore
            )

(* Otherwise, ignore the call.
Previously, if this method was called during a transition, we would skip the transition. Now we require Runner to call the force_complete_transition () function. That way, we explicitly complete the existing transition and start a new one.*)
    | _ ->
        do warn "update_cross_fade" false "Called with unexpected state. Ignoring." debug_data
        state, command

(* Show/hide *)

let private update_show
    (configuration : Fade_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    (state : Fade_State<'T>)
    (data : Show_Message_Data<'T>)
    : Fade_State<'T> * Cmd<Fade_Message<'T>> =

    let debug_data : (string * obj) list = ["state", state; "data", data :> obj; "transition_timeout_function_handle", transition_timeout_function_handle.current]

    let command =
        match data.is_notify_transition_complete, data.command_queue_item_id with
        | true, Some command_queue_item_id -> command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg
        | true, None -> error "update_show" "Show_Message_Data.is_notify_transition_complete is true, but Show_Message_Data.command_queue_item_id is None." ["Show_Message_Data", data] |> invalidOp
        | _ -> Cmd.none

    match state with

    | Idle_Hidden
    | Idle_Visible _ ->
        Idle_Visible data.data, command

    | Cross_Fade_Transition _
    | Fade_In_Transition _
    | Fade_Out_Transition _ ->
        cancel_transition_timeout_function configuration transition_timeout_function_handle debug_data
        Idle_Visible data.data, command

    | _ ->
        do warn "update_show" false "Called with unexpected state. Ignoring." debug_data
        state, command

let private update_hide
    (configuration : Fade_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    (state : Fade_State<'T>)
    (data : Hide_Message_Data<'T>)
    : Fade_State<'T> * Cmd<Fade_Message<'T>> =

    let debug_data : (string * obj) list = ["state", state; "transition_timeout_function_handle", transition_timeout_function_handle.current]

    let command =
        match data.is_notify_transition_complete, data.command_queue_item_id with
        | true, Some command_queue_item_id -> command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg
        | true, None -> error "update_hide" "Hide_Message_Data.is_notify_transition_complete is true, but Hide_Message_Data.command_queue_item_id is None." ["Hide_Message_Data", data] |> invalidOp
        | _ -> Cmd.none

    match state with

    | Idle_Hidden
    | Idle_Visible _ ->
        Idle_Hidden, command

    | Cross_Fade_Transition _
    | Fade_In_Transition _
    | Fade_Out_Transition _ ->
        cancel_transition_timeout_function configuration transition_timeout_function_handle debug_data
(* TODO2 There is a potential timing/stale data issue here. It takes different amounts of time to (1) update the state and (2) dispatch and handle Notify_Transition_Complete, which calls Runner_Transition.get_notify_transition_complete (), which calls Runner_Run.get_next_command ().

See notes in Fade_Transition.update_complete_transition (), Menu.update () and Runner_Transition.get_notify_transition_complete ().

We currently delay in Runner_Transition.get_notify_transition_complete () because the state updates too slowly.
- However, there have been other circumstances where this delay caused the state to update before we were ready, which resulted in other unwanted behavior. We cannot remember what it was at the moment.

- Maybe we should stop having Runner_State.get_components_with_running_transitions () query for the state and keep the state internal to each component.
But then we would have to:
1? Have Runner_Run keep a concurrent dictionary of the components to which it has dispensed commands, and have it remove each component from the list only when it receives a notification that that component's transition is complete.
2 Rely entirely on each component issuing a notification for each transition that completes.
3 Always have a component notify when it completes a transition.
20251118 We experimented to see if this is viable, by having Runner_Run.handle_command () handle the Fade_Out_All message by allowing all components - Dialogue_Box, Characters, and Background, to notify. However, that still causes unwanted behavior.
*)
        Idle_Hidden, command

    | _ ->
        do warn "update_show" false "Called with unexpected state. Ignoring." debug_data
        state, command

(* Update *)

let update
    (configuration : Fade_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    (notify_transition_complete : int<runner_queue_item_id> -> unit)
    (message : Fade_Message<'T>)
    (state : Fade_State<'T>)
    : Fade_State<'T> * Cmd<Fade_Message<'T>> =

    #if debug
    do
        debug "update" String.Empty ["debug_state_update_counter", debug_state_update_counter; "state", state; "msg", message]
        debug_state_update_counter <- debug_state_update_counter + 1
    #endif

    match message with

    | Fade_In fade_in_data -> update_fade_in configuration state fade_in_data transition_timeout_function_handle

    | Fade_Out transaction_time -> update_fade_out configuration state transaction_time transition_timeout_function_handle

    | Show data -> update_show configuration transition_timeout_function_handle state data 

    | Hide data -> update_hide configuration transition_timeout_function_handle state data

    | Cross_Fade cross_fade_data -> update_cross_fade configuration state cross_fade_data transition_timeout_function_handle

(* Fade_In, Fade_Out, and Cross_Fade are sent by the user. Begin_Transition and Complete_Transition are sent by timeout functions. *)
    | Begin_Fade_Transition -> update_begin_transition configuration transition_timeout_function_handle state

    | Complete_Fade_Transition -> update_complete_transition configuration state

    | Notify_Transition_Complete command_queue_item_id ->
        do notify_transition_complete command_queue_item_id
        state, Cmd.none
