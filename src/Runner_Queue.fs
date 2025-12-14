module Runner_Queue

// DateTime, String
open System

// console
open Browser.Dom
// IRefValue
open Fable.React

open Command_Types
open Log
open Runner_Helpers
open Runner_State
open Runner_Types
open Scripts
open Units_Of_Measure
open Utilities

(* Debug *)

let debug_module_name = "Runner_Queue"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

let initial_queue_id = 0<command_queue_item_id>

let mutable remove_transition_lock = 0

(* Functions - helpers *)

let get_initial_queue () : Command_Queue =
    
    Queue_Idle {
        next_command_data = {
            next_command_queue_item_id = initial_queue_id
            next_command_scene_id = entry_scene_id
            next_command_id = Some scene_initial_command_id
        }
(* We track the list of component IDs in use at the queue level so we can do the following.
- When the player interrupts, complete transitions for all components used by all commands in the queue.
- Ensure the player does not run commands in parallel that use the same component.
(end)
*)
        add_to_history = false
        menu_variables = Map.empty
    }

let private add_command_to_queue
    (queue_data : Command_Queue_State_Loading_Data)
    (command_data : Runner_Command_Data)
    : Command_Queue_State_Loading_Data =

    #if debug
    do debug "add_command_to_queue" String.Empty ["current_command", command_data.debug_data]
    #endif

(* This function is called only by add_commands_to_queue ().
add_commands_to_queue () checks that queue state is Queue_Idle or Queue_Loading.
We set continue_after_finished and add_to_history in add_commands_to_queue () after we have added all commands.
*)

    {
        commands = queue_data.commands.Add (queue_data.next_command_data.next_command_queue_item_id, {
            command_data = command_data
            order_in_queue = queue_data.commands.Count |> LanguagePrimitives.Int32WithMeasure
            components_used_by_command = command_data.components_used
        })
        next_command_data = {
            next_command_queue_item_id = queue_data.next_command_data.next_command_queue_item_id + 1<command_queue_item_id>
            next_command_scene_id = command_data.next_command_scene_id
            next_command_id = command_data.next_command_id
        }
        components_used_by_commands =
            if Set.empty <> Set.intersect queue_data.components_used_by_commands command_data.components_used then
                do warn "add_command_to_queue" false "Overlap between components used by command and components used by commands already in queue." ["Components used by commands already in queue", queue_data.components_used_by_commands; "command", command_data.debug_data; "Components used by command", command_data.components_used]
            Set.union queue_data.components_used_by_commands command_data.components_used
        menu_variables = queue_data.menu_variables
    }

let rec private add_commands_to_queue
    (queue : IRefValue<Command_Queue>)
    (scenes : Scene_Map)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

(* Change the queue state to Queue_Loading to protect it from other functions. *)
    let queue_data_1 =
        match queue.current with
        
        | Queue_Idle data ->
            {
                commands = Map.empty
                next_command_data = data.next_command_data
                components_used_by_commands = Set.empty
                menu_variables = data.menu_variables
            }

(* This function is recursive, so the queue state might already be Queue_Loading. *)
        | Queue_Loading data -> data

        | _ -> error "add_commands_to_queue" "Unexpected queue state." ["Queue state", queue.current] |> invalidOp

    let queue_state = Queue_Loading queue_data_1
    do queue.current <- queue_state

    #if debug
    do debug "add_commands_to_queue" String.Empty ["next_command_id", queue_data_1.next_command_data.next_command_id]
    #endif

(* Get the next command ID. *)
    match queue_data_1.next_command_data.next_command_id with

(* If there are no more commands to add to the queue... *)
    | None ->

(* If we have already loaded commands, run them. *)
        if queue_data_1.commands.Count > 0 then
            do queue.current <- Queue_Running {
                commands = queue_data_1.commands
                next_command_data = queue_data_1.next_command_data
                components_used_by_commands = queue_data_1.components_used_by_commands
                continue_after_finished = false
                add_to_history = false
                menu_variables = queue_data_1.menu_variables
            }
(* If we have not already loaded any commands, we are done. *)
        else do queue.current <- Queue_Done

    | Some next_command_id_2 ->

(* Get the scene for the next command. *)
        match scenes.TryFind queue_data_1.next_command_data.next_command_scene_id with

        | None -> error "add_commands_to_queue" "Next command scene ID not found." ["next_command_scene_id", queue_data_1.next_command_data.next_command_scene_id; "scenes", scenes] |> invalidOp

        | Some scene ->

(* Get the next command from its scene using the command ID. *)
            match scene.TryFind next_command_id_2 with

            | None -> error "add_commands_to_queue" "Next command ID not found in current scene." ["scene", queue_data_1.next_command_data.next_command_scene_id; "next_command_id", next_command_id_2] |> invalidOp

            | Some command ->

                let command_data = get_command_data queue_data_1.next_command_data.next_command_scene_id runner_components command queue_data_1.menu_variables
                let queue_data_2 = add_command_to_queue queue_data_1 command_data
                match command_data.behavior with

(* We keep adding commands to the queue until we reach a command with behavior Wait_For_Callback. *)
                | Continue_Immediately ->
                    do queue.current <- Queue_Loading queue_data_2
                    add_commands_to_queue queue scenes runner_components

(* Once we reach a command with behavior Wait_For_Callback, set continue_after_finished and add_to_history to reflect that command. *)
                | Wait_For_Callback wait ->
                    do queue.current <-
                        Queue_Running {
                            commands = queue_data_2.commands
                            next_command_data = queue_data_2.next_command_data
                            components_used_by_commands = queue_data_2.components_used_by_commands
                            continue_after_finished = wait.continue_afterward
(* Initially, we simply set add_to_history to the inverse of continue_after_finished. Later, if the player interrupts by showing the saved game screen or rolling back/forward, we set continue_after_finished to false, so we cannot determine the value of add_to_history later. *)
                            add_to_history = not wait.continue_afterward
                            menu_variables = queue_data_2.menu_variables
                        }

let private run_commands
    (queue : IRefValue<Command_Queue>)
    : unit =

    #if debug
    do debug "run_commands" String.Empty ["queue", queue.current]
    #endif

    match queue.current with

    | Queue_Running data ->

        #if debug
        do debug "run_commands" String.Empty ["commands", data.commands |> Seq.map (fun kv ->
        $"Command queue id: {kv.Key}. Command name: {kv.Value.command_data.debug_data}") |> Seq.toList :> obj]
        #endif

(* We check for no remaining commands both in add_commands_to_queue () and here.
If the last command has behavior Continue_Immediately, add_commands_to_queue () sets the queue state to Queue_Done.
If the last command has behavior Wait_For_Callback, remove_transition () removes it from the queue, then calls run (), which calls this function.
*)
        if Map.isEmpty data.commands then do queue.current <- Queue_Done
        else
(* We run all commands and then remove those with behavior Continue_Immediately and leave those with behavior Wait_For_Callback. The latter are removed by remove_transition () when their transitions complete. *)
            let commands =
                data.commands
                    |> Seq.sortBy (fun kv -> kv.Value.order_in_queue)
                    |> Seq.choose (fun kv ->
(* Run the command. *)
                        match kv.Value.command_data.command with
                        | Some f -> f kv.Key
                        | None -> ()
(* If the command does not wait for a transition to complete, remove it from the queue. *)
                        match kv.Value.command_data.behavior with
                        | Continue_Immediately -> None
                        | _ -> Some (kv.Key, kv.Value)
                    )
                    |> Map.ofSeq

(* If there are no commands that wait for a transition to complete, we are done. *)
            if Map.isEmpty commands then do queue.current <- Queue_Done
            else do queue.current <- Queue_Running { data with commands = commands }

    | _ -> error "run_commands" "Unexpected queue state." ["queue", queue.current] |> invalidOp

(* Functions - main *)

(* Ideally, queue would be a value, not a reference. But it must be a reference so it can also be accessed by remove_transition (), which is called by Runner_Transition.notify_transition_complete (), which in turn is called by various components. *)
let run
    (queue : IRefValue<Command_Queue>)
    (scenes : Scene_Map)
    (runner_components : IRefValue<Runner_Components>)
(* Previously, this was run_manually : bool. We used it to determine whether we were running a command automatically, or running (or interrupting) a command at player request. Now that is handled by queue.state. reason is for debugging.
*)
    (reason : Run_Reason)
    : unit =

    #if debug
    do debug "run" String.Empty <| ["reason", reason; "queue", queue.current]
    #endif

    match queue.current with
    | Queue_Idle _ ->
        do
            add_commands_to_queue queue scenes runner_components
            run_commands queue
    | Queue_Loading _ -> do error "run" "Unexpected queue state (Queue_Loading)." [] |> invalidOp
    | Queue_Running _ ->
        do force_complete_transitions runner_components queue false
            (fun () ->
                #if debug
                do debug "run" "Runner_State.force_complete_transitions () done." []
                #endif
                ()
            )
    | Queue_Interrupting _ -> do warn "run" false "Unexpected queue state (Queue_Interrupting)." []
(* If there are no more commands, ignore. *)
    | Queue_Done -> ()

let remove_transition
    (queue : IRefValue<Command_Queue>)
    (history : IRefValue<Runner_History>)
    (scenes : Scene_Map)
    (runner_components : IRefValue<Runner_Components>)
    (command_queue_item_id : int<command_queue_item_id>)
    (component_id : Runner_Component_Names)
    : unit =

    #if debug
    do debug "remove_transition" String.Empty ["component_id", component_id]
    #endif

(* When a component completes a transition, it signals the queue. This happens whether the transition completes on its own or the player interrupts it. *)
    match queue.current with
    | Queue_Running data
    | Queue_Interrupting data ->

(* Protect the map of running command queue items, and the set of running transitions for each command, from concurrent changes. *)
        lock (remove_transition_lock :> obj) (fun () ->
            let command_1 =
                match data.commands.TryFind command_queue_item_id with

                | None -> error "remove_transition" "Command queue item not found." ["command_queue_item_id", command_queue_item_id; "commands", data.commands |> Seq.map (fun kv ->
                $"Command queue id: {kv.Key}. Command: {kv.Value.command_data.debug_data}") |> Seq.toList :> obj] |> invalidOp

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

// TODO2 Consider break this function up here.
            let commands =
(* If the command queue item no longer contains any transitions, remove it. *)
                if command_1.components_used_by_command.IsEmpty then data.commands.Remove command_queue_item_id
(* Otherwise, update the command queue item with the new set of transition. *)
                else data.commands.Add (command_queue_item_id, command_1)

(* If there are no remaining running commands, set the queue state to Queue_Idle. *)
            if Map.isEmpty commands then
                queue.current <-
                    Queue_Idle {
                        next_command_data = data.next_command_data
                        add_to_history = data.add_to_history
                        menu_variables = data.menu_variables
                    }
(* We typically add the current state to the undo/redo history when the queue state is Queue_Idle, which implies we are waiting for player input. *)
                if data.add_to_history then
                    add_to_history history runner_components queue
(* Run the next command(s) if specified. *)
                if data.continue_after_finished then
                    run queue scenes runner_components Handle_Queue_Empty
            else
(* Update the queue with the new map of command queue items. *)
                do queue.current <-
                    match queue.current with
                    | Queue_Running data ->
                        Queue_Running { data with commands = commands }
                    | Queue_Interrupting data ->
                        Queue_Interrupting { data with commands = commands }
                    | _ -> error "remove_transition" "Unexpected queue state." ["queue", queue.current] |> invalidOp
        )

    | _ -> error "remove_transition" "Unexpected queue state." ["Queue state", queue.current] |> invalidOp
