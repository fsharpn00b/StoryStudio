module Runner_Transition

// String.Empty
open System

// window
open Browser.Dom
// IRefValue
open Feliz

open Log
open Runner_Types
open Utilities

(* Debug *)

let debug_module_name = "Runner_Transition"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Helper functions *)

let private component_id_to_component
    (runner_components : IRefValue<Runner_Components>)
    (component_id : Runner_Component_Names)
    : I_Transitionable =

    match component_id with
    | Background -> runner_components.current.background.current :?> I_Transitionable
    | Characters -> runner_components.current.characters.current :?> I_Transitionable
    | Dialogue_Box -> runner_components.current.dialogue_box.current :?> I_Transitionable
    | Menu -> runner_components.current.menu.current :?> I_Transitionable
    | Image_Map -> runner_components.current.image_map.current :?> I_Transitionable

(* Main functions *)

(* Setting the queue state to Queue_Interrupting is to prevent the player from making overlapping calls to this function by scrolling the mouse wheel rapidly. We would use lock (), but we want to discard such excess calls rather than run them in sequence.

force_complete_transitions () is called in the following cases.
1.1 The player opens the save/load game screen.
1.2 The player rolls back/forward.
1.3 The player interrupts a running command.
(end)

1.1 and 1.2 might overlap with 1.3. That is, the player might interrupt a running command by opening the save/load game screen or rolling back/forward. It does not matter if 1.1 or 1.2 happen while the queue state is Queue_Idle, Queue_Done, or Queue_Running. force_complete_transitions () is called in any of these cases.

For 1.1 and 1.2:
- override_continue_after_finished is true, which means continue_after_finished is always false.
(end)

For 1.3:
- override_continue_after_finished is false, which means continue_after_finished might be true or false.
(end)

force_complete_transitions () does the following.
2.1 Calls I_Transitionable.force_complete_transition () for each component used by the current command.
2.2 Each component calls Runner_Transition.get_notify_transition_complete ()
2.3 which calls Runner_Queue.remove_transition ()
2.4 which sets the queue state to Queue_Idle once all transitions are complete and all commands to which those transitions belong are removed.
(end)

The possible queue states are idle, done, running, loading, interrupting.

In case 1.3
- State loading results in an error.
- State interrupting results in a warning.
- State running calls force_complete_transitions ().
- State idle calls Runner_Queue.add_commands_to_queue () and Runner_Queue.run_commands ().
- State done does nothing.
See Runner_Queue.run ().

In case 1.1 or 1.2, we call force_complete_transitions () first. That puts the queue state to Queue_Idle.
As for force_complete_transitions ()
- State idle or done calls continuation ().
- State running stops transitions.
- State loading or interruping results in an error.

*

The following are notes on previous issues.

Previously, continuation () was responsible for setting the queue state from Queue_Interrupting to Queue_Idle. We are not sure why. It might be because:
3.1 We forgot that Runner_Queue.remove_transition () sets the queue state to Queue_Idle.
3.2 Runner_Queue.remove_transition () did not yet set the queue state to Queue_Idle, but left it in state Queue_Running or Queue_Interrupting.
3.3 Before that, force_complete_transitions () set the queue state to Queue_Idle before calling continuation (). However, in case 1.3, if queue.continue_after_finished is true:

3.3.1 We do not change it in force_complete_transitions ().
3.3.2 Runner_Queue.remove_transition () calls Runner_Queue.run ()
3.3.3 which calls Runner_Queue.run_commands ()
3.3.4 which, if the queue is not empty, sets the queue state to Queue_Running.
3.3.5 We think there was a race condition where force_complete_transitions () then overwrote the queue state with Queue_Idle.
3.3.6 Then, if the player interrupts again, Runner_Queue.run () finds queue state Queue_Idle (incorrect) and adds more commands (incorrect). If the queue state were Queue_Running (correct), it would interrupt (correct).

3.4 This might have happened because force_complete_transitions () was not actually waiting for all transitions to complete, because we were not yet using a continuation.
3.5 There was also a later issue where force_complete_transitions () was getting a stale result (true) from each component's is_transition_running () method, meaning it never called continuation ().
(end)

To address the possible race condition, we had force_complete_transitions () record the queue state (Queue_Idle or Queue_Running) and then restored it before calling continuation (). However, we worried that in case 1.3, the same race condition would occur, only in this case:
4.1 Assume queue state is Running.
4.2 Runner_Queue.handle_queue_empty () calls Runner_Queue.run ()
4.3 which calls Runner_Queue.run_commands ()
4.4 which, if the queue is empty, sets the queue state to Queue_Idle.
4.5 force_complete_transitions () overwrites the queue state with Queue_Running.
(end)
*)
(* 20251221 Where do we call Runner_State.get_state ()?
1 Runner_State.add_to_history (). That is called in Runner_Queue.remove_transition_2 () after there are no remaining running commands and we have just set the queue state to Queue_Idle.
2 Runner_State.quicksave_or_autosave (). We call force_complete_transitions () here.
3 Runner_State.export_current_game_to_file (). We call force_complete_transitions () here.
4 Runner_State.show_saved_game_screen (). We call force_complete_transitions () here.
(end)

x Check all calls to this and make sure the override_continue_after_finished parameter value is right. It is always set to true except for player interrupt. It looks right. It means that for player interrupt, we should go ahead and run the next command; for other events, listed below, we should not.
x Where do we call Runner_State.force_complete_transitions ()?
1 Runner_Queue.run () (Player interrupt)
2 Runner_State.undo_redo ()
3 Runner_State.show_saved_game_screen ()
4 Runner_State.show_configuration_screen ()
5 Runner_State.quicksave_or_autosave ()
6 Runner_State.export_current_game_to_file ()
7 Runner.import_current_game_from_file ()
(end)
*)
let force_complete_transitions
    (runner_components : IRefValue<Runner_Components>)
    (queue : IRefValue<Runner_Queue>)
    (override_continue_after_finished : bool)
    (continuation : unit -> unit)
    : unit =

    #if debug
    do debug "force_complete_transitions" String.Empty ["override_continue_after_finished", override_continue_after_finished; "queue", queue.current]
    #endif

    match queue.current with

(* If the queue state is Queue_Idle or Queue_Done, all transitions should be complete, so go ahead and run the continuation.
Runner_Queue.remove_transition () sets the queue state to Queue_Idle when a command with behavior Wait_For_Callback/continuer_after_finished = false is done.
Runner_Queue.remove_transition () and Runner_Queue.add_commands_to_queue () set the queue state to Queue_Done when no commands remain.
*)
    | Queue_Idle _
    | Queue_Done -> continuation ()

    | Queue_Running data ->

(* See notes before this function. *)
        do queue.current <- Queue_Interrupting {
            data with
(* If the player opened the save/load game screen or rolled back/forward, we need to make sure we do not continue to the next command(s) after we complete the current transition(s). *)
                continue_after_finished =
                    if override_continue_after_finished then false
                    else data.continue_after_finished
        }

        data.components_used_by_commands
            |> Set.toList
            |> List.map (component_id_to_component runner_components)
(* TODO2 Menu does not implement I_Transitionable because it does not have "real" transitions, only Show and Hide. For the same reason, it should not be possible to interrupt a menu transition. *)
            |> List.iter (fun (runner_component : I_Transitionable) ->
                #if debug
                do debug "force_complete_transitions" "Forcing completion for component." ["component", runner_component.get_name ()]
                #endif
                do runner_component.force_complete_transition ()
            )

        let rec wait_for_transitions_to_complete () : unit =
(* Get updated versions of these I_Transitionables. The old ones are closed over stale component state values.
We also now address this by using IRefValues for all component states.
*)
            let components_running_transitions =
                data.components_used_by_commands
                    |> Set.toList
                    |> List.map (component_id_to_component runner_components)
                    |> List.filter (fun runner_component-> runner_component.is_running_transition ())
(* is_running_transition () checks the internal state of each component, which in our experience is the last thing to be updated (that is, after the runner command queue has been notified), so this should be the safest way to make sure the transition is complete. *)
            if components_running_transitions.Length > 0 then
                #if debug
                do debug "wait_for_transitions_to_complete" "Waiting for components to complete transitions." ["Components", components_running_transitions |> List.map (fun runner_component -> runner_component.get_name ()) :> obj]
                #endif

                do window.setTimeout ((fun () ->
                    wait_for_transitions_to_complete ()
                ), int wait_for_transitions_to_complete_time) |> ignore
            else
                continuation ()

        wait_for_transitions_to_complete ()

    | _ -> error "force_complete_transitions" "Unexpected queue state." ["queue", queue.current] |> invalidOp
