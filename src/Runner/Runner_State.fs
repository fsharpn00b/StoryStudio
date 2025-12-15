module Runner_State

// String.Empty
open System

// window
open Browser.Dom
open Feliz
// Decode, Encode
open Thoth.Json

open Configuration
open JavaScript_Interop
open Log
open Runner_Helpers
open Runner_Types
open Save_Load_Types
open Scripts
open Utilities

(* Debug *)

let debug_module_name = "Runner_State"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Helper functions - state *)

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
let force_complete_transitions
    (runner_components : IRefValue<Runner_Components>)
    (queue : IRefValue<Command_Queue>)
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

(* Main functions - state *)

let get_state
    (runner_components : IRefValue<Runner_Components>)
(* Previously this parameter was type Command_State rather than IRefValue<Command_State>, meaning we called .current before passing the result to this function. It seems that caused this function to see a stale state. It seems the rule is, with any IRefValue, we must call .current as late as possible, whether setting or getting or the value. *)
    (queue : IRefValue<Command_Queue>)
    : Runner_Saveable_State =

    match queue.current with
    | Queue_Idle data ->

        Runner_Saveable_State_Running {
            next_command_queue_item_id = data.next_command_data.next_command_queue_item_id
            scene_id = data.next_command_data.next_command_scene_id
            next_command_id = data.next_command_data.next_command_id
(* We clear the history when we load a saved game. When we load a saved game, we need to determine whether to re-add the current state to the history.
We complete all running transitions before we show the save/load game screen. Any commands with behavior Continue_Immediately should have finished. In effect, the queue should have been in a Wait_For_Callback state. So when we load a saved game, we only need to check add_to_history, which is simply the inverse of continue_after_finished. We do not use continue_after_finished itself because we need to set it to false to halt running commands when the user opens the save/load game screen or rolls back/forward.
If add_to_history is false, which means that when we halted running commands, we would otherwise have run the next command without waiting for input from the user, we should not add the current state to the history.
*)
            add_to_history = data.add_to_history
            component_data = {
                background = runner_components.current.background.current.get_state ()
                dialogue = runner_components.current.dialogue_box.current.get_state ()
                characters = runner_components.current.characters.current.get_state ()
                menu = runner_components.current.menu.current.get_state ()
                javascript = get_state_from_js ()
            }
            menu_variables = data.menu_variables
        }

    | Queue_Done ->

        Runner_Saveable_State_Done {
            background = runner_components.current.background.current.get_state ()
            dialogue = runner_components.current.dialogue_box.current.get_state ()
            characters = runner_components.current.characters.current.get_state ()
            menu = runner_components.current.menu.current.get_state ()
            javascript = get_state_from_js ()
        }

(* At this point, we should have forced transitions to complete, or already have been in state Queue_Idle. *)
    | _ -> error "get_state" "Unexpected queue state." ["Queue state", queue.current] |> invalidOp

(* Each UI component is responsible to notify its container (Runner) that a transition is complete. This is because some components (Characters, Dialogue_Box) might handle multiple transitions (multiple characters, fade and typing transitions, respectively). We want the component to send only a single notification for these grouped transitions, because Runner does not concern itself with the individual transitions - those should be contained by the component.
*)
let set_state
    (runner_state : Runner_Saveable_State)
    (queue : IRefValue<Command_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

    let component_data =
        match runner_state with
        | Runner_Saveable_State_Running data -> data.component_data
        | Runner_Saveable_State_Done data -> data

    do
(* See the notes in Runner.fs, case 3, for how this works. *)
(* A UI component is responsible for completing its transitions before setting its state. *)
(* TODO2 Consider calling Runner_Run.force_complete_transitions () here instead.
- Either way, do we need a delay or callback after each component completes its transition, as we use callbacks for Runner_Run.force_complete_transitions ()/get_next_command ()?
*)
(* TODO2 When we save (that is, call each component's get_state () method), we record the final state of any in-progress transition, rather than the original state.
We might want to get the state at the most recent pausable point (which might be the current point if no transition is in progress). Since we plan to represent history and save state the same way, this should be feasible. However, if they have just loaded the game, the history might be empty.
*)
        runner_components.current.background.current.set_state component_data.background
        runner_components.current.dialogue_box.current.set_state component_data.dialogue
        runner_components.current.characters.current.set_state component_data.characters
        runner_components.current.menu.current.set_state component_data.menu
        set_state_in_js component_data.javascript

(* We set the command state afterward to prevent having it overwritten due to a component completing an existing transition (on its own, not because we called its set_state () method) after we set the command state but before we call the components' set_state () methods.
*)
        queue.current <-
            match runner_state with
            | Runner_Saveable_State_Running data ->
                Queue_Idle {
                    next_command_data = {
                        next_command_queue_item_id = data.next_command_queue_item_id
                        next_command_scene_id = data.scene_id
                        next_command_id = data.next_command_id
                    }
                    add_to_history = data.add_to_history
                    menu_variables = data.menu_variables
                }
            | Runner_Saveable_State_Done _ -> Queue_Done

(* TODO2 Should we call get_next_command () at the end of set_state ()?
See also TODO 2 item at the end of Runner_Run.fs.

N Maybe, when we save the game, we should check the history and retreat to the last pause point.
20251109 We don't want to now, because set_state is now called not only by load_game () but by undo_redo (). Calling get_next_command () can change the history, which we don't want to do while moving around in it.
- Might need a flag that distinguishes these scenarios.

- We should have a list of commands that we pause on, or a function that takes a command and returns true or false. We can also use that list to decide whether to call get_next_command ()? And add_to_history ()? The function would need to take Command_Post_Parse_Type, because Menu and Command are both Command_Post_Parse_Type. Then it would need to match on Command and look at the subtypes/cases.

It's embedded in the commands already? Yes, it's embedded in Command, but Menu and If and End If (along with Command) are in Command_Post_Parse_Type.
Dialogue is a command and has behavior Wait_For_Callback {| continue_afterward = false |}
How do we encode behavior for Menu? In Runner_Run.handle_menu ().
*)

let set_configuration
    (runner_components : IRefValue<Runner_Components>)
    (old_configuration : IRefValue<Runner_Configuration>)
    (new_configuration : Runner_Configuration)
    : unit =
    do
        runner_components.current.background.current.set_configuration new_configuration.background_configuration
        runner_components.current.characters.current.set_configuration new_configuration.characters_configuration
        runner_components.current.dialogue_box.current.set_configuration new_configuration.dialogue_box_configuration
        old_configuration.current <- new_configuration

let add_to_history
    (history : IRefValue<Runner_History>)
    (runner_components : IRefValue<Runner_Components>)
    (queue : IRefValue<Command_Queue>)
    : unit =
    do history.current <-
        match history.current.current_index with
        | None ->
            {
                current_index = Some 0
                history = [get_state runner_components queue]
            }
        | Some current_index ->
            {
                current_index = Some <| current_index + 1
                history = (List.take (current_index + 1) history.current.history) @ [get_state runner_components queue]
            }

(* TODO2 We have not tried to redo during a transition. That is possible, as we only truncate the history when we add to it. In other words, we could:
1 Run a command (a) that uses a transition.
2 Reach a pause point (b).
3 Undo to (a).
4 Click to progress from (a) to (b), running the transaction again.
5 Redo to (b), which should still be in our history.
(end)
*)
let undo_redo
    (history : IRefValue<Runner_History>)
    (queue : IRefValue<Command_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (undo : bool)
    : unit =

    #if debug
    do debug "undo_redo" String.Empty ["History index (before)", history.current.current_index; "History length", history.current.history.Length]
    #endif

    match history.current.current_index with
    | Some current_index_1 ->
        if (undo && current_index_1 > 0) || (not undo && current_index_1 < history.current.history.Length - 1) then
(* We do not call force_complete_transitions () until we have determined undo/redo is valid. *)
            do force_complete_transitions runner_components queue true (fun () ->
                #if debug
                do debug "undo_redo" "Runner_State.force_complete_transitions () done." []
                #endif

                let current_index_2 = if undo then current_index_1 - 1 else current_index_1 + 1
                do
                    history.current <- { history.current with current_index = Some current_index_2 }

                    #if debug
                    debug "undo_redo" String.Empty ["History index (after)", history.current.current_index; "History length", history.current.history.Length]
                    #endif

                    set_state history.current.history.[current_index_2] queue runner_components
            )
    | None -> ()

(* Main functions - save and load games *)

// TODO2 This transcends Runner? No, configuration should be stored separately anyway.
let private load_game
    (history : IRefValue<Runner_History>)
    (queue : IRefValue<Command_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (saved_game : string)
    : unit =

    match Decode.Auto.fromString<Runner_Saveable_State> saved_game with
    | Ok saved_state ->
        do
(* Clear the history. *)
            history.current <- { current_index = None; history = [] }
(* We do not need to call force_complete_transitions () because we did so before showing the saved game screen.
Each UI component's set_state () method dispatches Show or Hide messages with notify_transition_complete = false to prevent commands from auto-continuing unexpectedly.
*)
            set_state saved_state queue runner_components
(* If the state was saved at a point where it should be added to the history (typically, this means a point where we are waiting for player input), add the state to the history again. We need to do this after calling set_state (), because add_to_history () calls get_state ().
*)
(* See also Runner_Transition.notify_transition_complete (). *)
            match saved_state with
            | Runner_Saveable_State_Running data when data.add_to_history ->
(* Include a delay to make sure set_state finishes updating runner_components and command_state. *)
                window.setTimeout((fun () ->
                    add_to_history history runner_components queue
                ), int notify_transition_complete_delay_time) |> ignore
            | _ -> ()
    | _ -> error "load_game" "Failed to deserialize saved game." ["saved_game", saved_game] |> invalidOp

let get_load_game
    (runner_components : IRefValue<Runner_Components>)
    (history : IRefValue<Runner_History>)
    (queue : IRefValue<Command_Queue>)
    : string -> unit =
(* Close load_game () over all these parameters so it becomes a string -> unit that we can pass to UI components.
We also need to delay the evaluation of this function until runner_components_1 is not None. The delayed result of this function is passed to the constructors of these components.
We can close over command_state because it is a reference.
*)
    fun (saved_game : string) -> load_game history queue runner_components saved_game

let show_saved_game_screen
    (queue : IRefValue<Command_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (action : Saved_Game_Action)
    : unit =

    if runner_components.current.save_load.current.is_visible () then
        do runner_components.current.save_load.current.switch action
    else
(* The save/load and configuration screens cannot be open at the same time. *)
        if runner_components.current.configuration.current.is_visible () then
            do runner_components.current.configuration.current.hide ()
(* Stop all transitions before we show the saved game screen. *)
        do force_complete_transitions runner_components queue true (fun () ->
            #if debug
            do debug "show_saved_game_screen" "Runner_State.force_complete_transitions () done." []
            #endif

            let runner_state = get_state runner_components queue
            let json = Encode.Auto.toString (0, runner_state)
            do runner_components.current.save_load.current.show action json
        )
        
let hide_saved_game_screen
    (runner_components : IRefValue<Runner_Components>)
    : unit =

    if runner_components.current.save_load.current.is_visible () then
        do runner_components.current.save_load.current.hide ()

(* Main functions - UI *)

let show_configuration_screen
    (queue : IRefValue<Command_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

(* The save/load and configuration screens cannot be open at the same time. *)
    if runner_components.current.save_load.current.is_visible () then
        do runner_components.current.save_load.current.hide ()
    do force_complete_transitions runner_components queue true (fun () ->
        do runner_components.current.configuration.current.show ()
    )

let show_or_hide_ui
    (runner_components : IRefValue<Runner_Components>)
    : unit =

(* We do not request notification when the transition completes, or provide a command queue item ID, because this is an internal command. *)
    if runner_components.current.dialogue_box.current.is_visible () then do runner_components.current.dialogue_box.current.hide false None
    else do runner_components.current.dialogue_box.current.show false None
