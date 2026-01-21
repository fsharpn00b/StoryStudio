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
open Runner_Types
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Queue_Transition"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

let mutable remove_transition_lock = 0

(* Main functions *)

let private remove_transition_2
    (queue : IRefValue<Runner_Queue>)
    (history : IRefValue<Runner_History>)
    (scenes : Scene_Map)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (queue_data : Runner_Queue_State_Running_Data)
    (command_queue_item_id : int<runner_queue_item_id>)
    (command : Runner_Queue_Item)
    : unit =

    let commands =
(* If the command queue item no longer contains any transitions, remove it. *)
        if command.components_used_by_command.IsEmpty then queue_data.commands.Remove command_queue_item_id
(* Otherwise, update the command queue item with the new set of transitions. *)
        else queue_data.commands.Add (command_queue_item_id, command)

(* If there are no remaining running commands, set the queue state to Queue_Idle. *)
    if Map.isEmpty commands then
        queue.current <-
            Queue_Idle {
                next_command_data = queue_data.next_command_data
                add_to_history = queue_data.add_to_history
                autosave = queue_data.autosave
                menu_variables = queue_data.menu_variables
            }
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
        if queue_data.add_to_history then
            add_to_history history runner_component_interfaces queue
        if queue_data.autosave then
            quicksave_or_autosave queue runner_component_interfaces Save_Load_Types.Autosave
(* Run the next command(s) if specified. *)
        if queue_data.continue_after_finished then
            run queue scenes runner_component_interfaces Handle_Queue_Empty
    else
(* Update the queue with the new map of command queue items. *)
        do queue.current <-
            match queue.current with
            | Queue_Running data ->
                Queue_Running { data with commands = commands }
            | Queue_Interrupting data ->
                Queue_Interrupting { data with commands = commands }
            | _ -> error "remove_transition" "Unexpected queue state." ["queue", queue.current] |> invalidOp

let remove_transition_1
    (queue : IRefValue<Runner_Queue>)
    (history : IRefValue<Runner_History>)
    (scenes : Scene_Map)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (command_queue_item_id : int<runner_queue_item_id>)
    (component_id : Runner_Component_Names)
    : unit =

    #if debug
    do debug "remove_transition" String.Empty ["component_id", component_id]
    #endif

(* When a component completes a transition, it signals the queue. This happens whether the transition completes on its own or the player interrupts it. *)
    match queue.current with
    | Queue_Running queue_data
    | Queue_Interrupting queue_data ->

(* Protect the map of running command queue items, and the set of running transitions for each command, from concurrent changes. *)
        lock (remove_transition_lock :> obj) (fun () ->
            let command_1 =
                match queue_data.commands.TryFind command_queue_item_id with

                | None -> error "remove_transition" "Command queue item not found." ["command_queue_item_id", command_queue_item_id; "commands", queue_data.commands |> Seq.map (fun kv ->
                $"Command queue id: {kv.Key}. Command: {kv.Value.command_data.debug_data}") |> Seq.toList :> obj] |> invalidOp

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
            remove_transition_2 queue history scenes runner_component_interfaces queue_data command_queue_item_id command_1
        )

    | _ -> error "remove_transition" "Unexpected queue state." ["Queue state", queue.current] |> invalidOp
