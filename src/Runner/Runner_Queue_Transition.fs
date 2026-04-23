module Runner_Queue_Transition

// console, window
open Browser.Dom
// IRefValue
open Feliz

open Command_Types
open Log
open Runner_History
open Runner_Queue
open Runner_Save_Load
open Runner_Types_2
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Queue_Transition"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

let mutable remove_transition_lock = 0

type Transition_Completion_Result = {
    next_queue : Runner_Queue
    should_add_to_history : bool
    should_autosave : bool
    should_continue_after_finished : bool
}

let compute_transition_completion
    (current_queue_state : Runner_Queue)
    (queue_data : Runner_Queue_State_Running_Data)
    (remaining_commands : Runner_Queue_Command_Map)
    : Transition_Completion_Result =

    if Map.isEmpty remaining_commands then
        {
            next_queue =
                Queue_Idle {
                    next_command_data = queue_data.next_command_data
                    add_to_history = queue_data.add_to_history
                    autosave = queue_data.autosave
                    menu_variables = queue_data.menu_variables
                }
            should_add_to_history = queue_data.add_to_history
            should_autosave = queue_data.autosave
            should_continue_after_finished = queue_data.continue_after_finished
        }
    else
        let next_queue =
            match current_queue_state with
            | Queue_Running _ -> Queue_Running { queue_data with commands = remaining_commands }
            | Queue_Interrupting _ -> Queue_Interrupting { queue_data with commands = remaining_commands }
            | _ -> error "compute_transition_completion" "Unexpected queue state." ["queue", current_queue_state] |> invalidOp
        {
            next_queue = next_queue
            should_add_to_history = false
            should_autosave = false
            should_continue_after_finished = false
        }

(* Main functions *)

let private remove_transition_2
    (runner_state : Runner_State)
    (history : IRefValue<Runner_History>)
    (queue_data : Runner_Queue_State_Running_Data)
    (command_queue_item_id : int<command_queue_item_id>)
    (command : Runner_Queue_Item)
    : unit =

    let commands =
(* If the command queue item no longer contains any transitions, remove it. *)
        if command.components_used_by_command.IsEmpty then queue_data.commands.Remove command_queue_item_id
(* Otherwise, update the command queue item with the new set of transitions. *)
        else queue_data.commands.Add (command_queue_item_id, command)

    let completion_result = compute_transition_completion runner_state.queue.current queue_data commands
    runner_state.queue.current <- completion_result.next_queue

(* If there are no remaining running commands, set the queue state to Queue_Idle. *)
    if Map.isEmpty commands then
(* While the queue state is Queue_Idle,
- Add the current state to the undo/redo history.
- Autosave.
(end)
*)
(* TODO2 Potential issues.
- We add commands to the queue until we reach a command with behavior Wait_For_Callback (regardless of whether continue_afterward is true), then we add that command to the queue, stop, and run the commands in the queue until it is empty. Entangling these two things (waiting for callback and emptying the queue) works for now, but we might want more flexible behavior in the future.
- When the queue is empty, we add to history and autosave. Again, entangling these two things might not be flexible enough in the future.
- It's not intuitive to remember that this function (remove_transition) is where we add to history and autosave.
*)
        if completion_result.should_add_to_history then
            add_to_history runner_state history
        if completion_result.should_autosave then
            autosave_or_quicksave runner_state Save_Load_Types.Autosave
(* Run the next command(s) if specified. *)
        if completion_result.should_continue_after_finished then
            run runner_state Handle_Queue_Empty

let remove_transition_1
    (runner_state : Runner_State)
    (history : IRefValue<Runner_History>)
    (command_queue_item_id : int<command_queue_item_id>)
    (component_id : Runner_Component_Names)
    : unit =

    #if debug
    do debug "remove_transition" String.Empty ["component_id", component_id]
    #endif

(* When a component completes a transition, it signals the queue. This happens whether the transition completes on its own or the player interrupts it. *)
    match runner_state.queue.current with
    | Queue_Running queue_data
    | Queue_Interrupting queue_data ->

(* Protect the map of running command queue items, and the set of running transitions for each command, from concurrent changes. *)
        lock (remove_transition_lock :> obj) (fun () ->
            let command_1 =
                match queue_data.commands.TryFind command_queue_item_id with

                | None ->
                    error
                        "remove_transition"
                        "Command queue item not found."
                        [
                            "command_queue_item_id", command_queue_item_id
                            "known_commands", queue_data.commands |> Seq.map (fun kv ->
                                $"command_queue_item_id: {kv.Key}. command: {kv.Value.command_data.error_data.source}"
                            ) |> Seq.toList :> obj
                        ] |> invalidOp

(* Remove the transition (as represented by its component ID) from the command. *)
                | Some command ->
                    let command_2 =
                        if command.components_used_by_command.Contains component_id then
                            { command with components_used_by_command = command.components_used_by_command.Remove component_id }
                        else
(* TODO2 We only warn here because there is a potential race condition between completing a transition and interrupting it. The trouble is, by that logic, we should only warn if the command queue item is not found. *)
                            warn "remove_transition" false "Transition not found." ["command_queue_item_id", command_queue_item_id; "component_id", component_id]
                            command

                    #if debug
                    do debug "remove_transition" String.Empty ["Transitions remaining", command_2.components_used_by_command ]
                    #endif

                    command_2

(* If the command has no more transitions, remove it from the queue. *)
            remove_transition_2 runner_state history queue_data command_queue_item_id command_1
        )

    | _ -> error "remove_transition" "Unexpected queue state." ["Queue state", runner_state.queue.current] |> invalidOp
