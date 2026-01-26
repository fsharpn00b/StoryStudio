module Transition

// console, window
open Browser.Dom
// Cmd
open Elmish
// IRefValue
open Feliz

open Transition_Types
open Log
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Transition"

let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1

let mutable private debug_state_update_counter = 1

(* Helpers*)

let cancel_transition_timeout_function
    (configuration : Transition_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    (debug_data : (string * obj) list)
    : unit =
    match transition_timeout_function_handle.current with
    | Some id ->
        do
            window.clearTimeout id
            transition_timeout_function_handle.current <- None
    | None -> do warn "cancel_transition_timeout_function" false "Tried to cancel transition timeout function, but the function was not running." <| debug_data

let is_running_transition (state : IRefValue<Transition_State<'data_type, 'transition_type>>) : bool =
    match state.current with
    | Idle _ -> false
    | Pre_Transition _
    | In_Transition _ -> true

(* Note This function forces the completion of a transition. *)
let force_complete_transition
    (state_1 : IRefValue<Transition_State<'data_type, 'transition_type>>)
    (dispatch : Transition_Message<'data_type, 'transition_type> -> unit)
    : unit =

    do
        match state_1.current with
        | Pre_Transition state_2
        | In_Transition state_2 ->
            dispatch <| Skip_Transition {
                new_data = state_2.new_data
                transition_type = state_2.transition_type
                is_notify_transition_complete = true
                command_queue_item_id = Some state_2.command_queue_item_id
            }

        | _ -> ()

(* Main functions *)

let private update_transition<'data_type, 'transition_type when 'data_type : equality>
    (configuration : Transition_Configuration)
    (state : Transition_State<'data_type, 'transition_type>)
    (message_data : Transition_Message_Data<'data_type, 'transition_type>)
    (transition_timeout_function_handle : IRefValue<float option>)
    : Transition_State<'data_type, 'transition_type> * Cmd<Transition_Message<'data_type, 'transition_type>> =

    let debug_data : (string * obj) list = ["state", state; "transition_timeout_function_handle", transition_timeout_function_handle.current]

    let command = message_data.command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg 

    match state with

(* It seems we need a delay after handling every message, even when the state does not change. Otherwise, we get behavior such as
- Commands that should automatically run the next command afterward, but do not.
- Commands after which we cannot run the next command even manually.
- Infinite loops, though we are not sure why. See also the notes for update_complete_transition (), but the infinite loops we encountered did not even involve changing the state.

TODO2 Maybe Notify_Transition_Complete should always delay. We cannot think of a case where we can safely set delay to false.
*)

(* If the old and new data are the same, do nothing. *)
    | Idle old_data when old_data = message_data.new_data ->
        state, command

    | Idle old_data ->
(* If the transition time is 0, just replace the old data. *)
        if message_data.transition_time <= 0.0<seconds> then
            Idle message_data.new_data, command
        else
(* Trigger the first render, to set up the old data before transitioning to the new data. *)
            #if debug
            do debug "update_transition" "Starting Pre_Transition -> In_Transition timeout." <| debug_data @ ["old_data", old_data]
            #endif
            Pre_Transition {
                old_data = old_data
                new_data = message_data.new_data
                transition_type = message_data.transition_type
                transition_time = message_data.transition_time
                command_queue_item_id = message_data.command_queue_item_id
            },
            Cmd.ofEffect (fun dispatch ->
                window.setTimeout((fun () ->
                    #if debug
                    do debug "Pre_Transition -> In_Transition timeout" String.Empty <| debug_data @ ["old_data", old_data]
                    #endif
                    dispatch Begin_Transition
                ), int pre_transition_time) |> ignore
            )

(* Otherwise, ignore the call.
Previously, if this method was called during a transition, we would skip the transition. Now we require Runner to call the force_complete_transition () function. That way, we explicitly complete the existing transition and start a new one.*)
    | _ ->
        do warn "update_transition" false "Called with unexpected state. Ignoring." debug_data
        state, command

let private update_skip_transition
    (configuration : Transition_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    (state : Transition_State<'data_type, 'transition_type>)
    (message_data : Skip_Transition_Message_Data<'data_type, 'transition_type>)
    : Transition_State<'data_type, 'transition_type> * Cmd<Transition_Message<'data_type, 'transition_type>> =

    let debug_data : (string * obj) list = ["state", state; "transition_timeout_function_handle", transition_timeout_function_handle.current]

    let command =
        match message_data.is_notify_transition_complete, message_data.command_queue_item_id with
        | true, Some command_queue_item_id -> command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg
        | true, None -> error "update_skip_transition" "Skip_Transition_Message_Data.is_notify_transition_complete is true, but Skip_Transition_Message_Data.command_queue_item_id is None." ["Skip_Transition_Message_Data", message_data] |> invalidOp
        | _ -> Cmd.none

    match state with

    | Idle _ ->
(* Replace the old data. *)
        Idle message_data.new_data, command

// TODO2 If this is used to force transition completion, then In_Transition data should equal message_data.new_data? In the former implementation (Fade_Visibility.update_show()/update_hide()), we just replaced the old data with message_data.new_data. We do the same here.
    | In_Transition _ ->
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
        Idle message_data.new_data, command

    | _ ->
        do warn "update_show" false "Called with unexpected state. Ignoring." debug_data
        state, command

let update_begin_transition
    (configuration : Transition_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    (state : Transition_State<'data_type, 'transition_type>)
    : Transition_State<'data_type, 'transition_type> * Cmd<Transition_Message<'data_type, 'transition_type>> =

    match state with

    | Pre_Transition transition_data ->
        #if debug
        do debug "update_begin_transition" "Starting Transition -> Idle timeout." ["state", state; "transition_data", transition_data]
        #endif
        In_Transition transition_data,
(* TODO2 How does this know the signature of dispatch? Does it infer it from the function return type?
Fade_State<'T> * Cmd<Fade_Msg<'T>>
*)
            Cmd.ofEffect (fun dispatch ->
                do transition_timeout_function_handle.current <- Some <| window.setTimeout((fun () ->
                    #if debug
                    do debug "Transition -> Idle timeout" String.Empty ["state", state; "transition_data", transition_data]
                    #endif
                    dispatch Complete_Transition
                ), int (transition_data.transition_time * 1000.0))
            )

    | _ ->
        do warn "update_begin_transition" false "Called with unexpected state. Ignoring." ["state", state]
        state, Cmd.none

(* This does not cancel the transition timeout because the call to it is dispatched by the timeout itself. *)
let update_complete_transition
    (configuration : Transition_Configuration)
    (state : Transition_State<'data_type, 'transition_type>)
    : Transition_State<'data_type, 'transition_type> * Cmd<Transition_Message<'data_type, 'transition_type>> =

    match state with

    | In_Transition transition_data ->
        #if debug
        do debug "update_complete_transition" "Completing In_Transition." ["state", state; "transition_data", transition_data]
        #endif
(* Notify the UI component container (Runner) that the transition is complete, so it can run the next command if needed.
We must do this after a delay. The state should be updated before the command runs, but it seems it is not, due to the asynchronous nature of React.
If we do not delay the dispatch of the message, we enter an infinite loop.
1 We dispatch message Notify_Transition_Complete.
2 Notify_Transition_Complete is handled by Transition.update ().
3 That calls the notify_transition_complete () closure that was passed to the constructor of the component that uses Transition.
4 That calls Runner_Run.get_next_command ().
5 That calls Runner_Run.force_complete_transitions ().
6 That calls <UI component>.is_running_transition ().
7 That returns true because the state is still In_Transition.
8 Runner_Run.force_complete_transitions () calls <UI component>.force_complete_transition ().
9 That calls Transition.force_complete_fade_transition () with notify_transition_complete = true.
10 That dispatches Skip_Transition, with notify_transition_complete = true.
11 That is handled by Transition.update_skip_transition ().
12 That dispatches message Notify_Transition_Complete (step 1).

For now, any time we change the state, we dispatch message Notify_Transition_Complete with delay set to true.

20251116 The delay is no longer needed due to the restructuring of Runner so it can wait for multiple transitions to complete before it runs the next command.
20251118 The delay is still needed. We now always delay in Runner_Transition.get_notify_transition_complete (). See notes in Menu.update () and Runner_Transition.get_notify_transition_complete ().
*)
        Idle transition_data.new_data, transition_data.command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg

(* Complete_Fade_Transition is only dispatched by transition timeout functions, so this means a transition completed on its own rather than being forced to complete by the player. force_complete_fade_transition (), which forces the completion of a transition, does not dispatch Complete_Fade_Transition. *)
    | _ ->
        do warn "update_complete_transition" false "Called with unexpected state. Ignoring." ["state", state]
        state, Cmd.none

let update
    (configuration : Transition_Configuration)
    (transition_timeout_function_handle : IRefValue<float option>)
    (notify_transition_complete : int<runner_queue_item_id> -> unit)
    (message : Transition_Message<'data_type, 'transition_type>)
    (state : Transition_State<'data_type, 'transition_type>)
    : Transition_State<'data_type, 'transition_type> * Cmd<Transition_Message<'data_type, 'transition_type>> =

    #if debug
    do
        debug "update" String.Empty ["debug_state_update_counter", debug_state_update_counter; "state", state; "msg", message]
        debug_state_update_counter <- debug_state_update_counter + 1
    #endif

    match message with

    | Transition new_data -> update_transition configuration state new_data transition_timeout_function_handle

    | Skip_Transition new_data -> update_skip_transition configuration transition_timeout_function_handle state new_data 

(* Transition and Skip_Transition are sent by Runner. Begin_Transition and Complete_Transition are sent by timeout functions. *)
    | Begin_Transition -> update_begin_transition configuration transition_timeout_function_handle state

    | Complete_Transition -> update_complete_transition configuration state

    | Notify_Transition_Complete command_queue_item_id ->
        do notify_transition_complete command_queue_item_id
        state, Cmd.none
