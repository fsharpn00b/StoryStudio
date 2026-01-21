module Runner_Queue

// String
open System

// console, window
open Browser.Dom
// IRefValue
open Feliz

open Command_Types
open Log
open Runner_Queue_Helpers
open Runner_Transition
open Runner_Types
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Queue"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

let initial_queue_id = 0<runner_queue_item_id>

(* Functions - helpers *)

let get_initial_queue () : Runner_Queue =
    
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
        autosave = false
        menu_variables = Map.empty
    }

let private add_command_to_queue
    (queue_data : Runner_Queue_State_Loading_Data)
    (command_data : Runner_Command_Data)
    : Runner_Queue_State_Loading_Data =

    #if debug
    do debug "add_command_to_queue" String.Empty ["current_command", command_data.debug_data]
    #endif

(* This function is called only by add_commands_to_queue ().
add_commands_to_queue () checks that queue state is Queue_Idle or Queue_Loading.
We set continue_after_finished and add_to_history in add_commands_to_queue () after we have added all commands.
We set autosave here because it can be set by commands with either type of behavior, and once it is set to true, we need to retain that value until we have added all commands.
*)
    {
        commands = queue_data.commands.Add (queue_data.next_command_data.next_command_queue_item_id, {
            command_data = command_data
            order_in_queue = queue_data.commands.Count |> LanguagePrimitives.Int32WithMeasure
            components_used_by_command = command_data.components_used
        })
        next_command_data = {
            next_command_queue_item_id = queue_data.next_command_data.next_command_queue_item_id + 1<runner_queue_item_id>
            next_command_scene_id = command_data.next_command_scene_id
            next_command_id = command_data.next_command_id
        }
        components_used_by_commands =
            if Set.empty <> Set.intersect queue_data.components_used_by_commands command_data.components_used then
                do warn "add_command_to_queue" false "Overlap between components used by command and components used by commands already in queue." ["Components used by commands already in queue", queue_data.components_used_by_commands; "command", command_data.debug_data; "Components used by command", command_data.components_used]
            Set.union queue_data.components_used_by_commands command_data.components_used
        autosave =
            queue_data.autosave ||
                match command_data.behavior with
                | Continue_Immediately data -> data.autosave
                | Wait_For_Callback data -> data.autosave
        menu_variables = queue_data.menu_variables
    }

let private handle_no_more_commands 
    (queue : IRefValue<Runner_Queue>)
    (queue_data : Runner_Queue_State_Loading_Data)
    : unit =

(* If we have already loaded commands, get ready to run them. *)
    if queue_data.commands.Count > 0 then
        do queue.current <- Queue_Running {
            commands = queue_data.commands
            next_command_data = queue_data.next_command_data
            components_used_by_commands = queue_data.components_used_by_commands
(* After we run the currently loaded commands, the game is over, so these values should all be false, even if they might normally be true. *)
            continue_after_finished = false
            add_to_history = false
            autosave = false
            menu_variables = queue_data.menu_variables
        }
(* If we have not already loaded any commands, we are done. *)
    else do queue.current <- Queue_Done

let rec private handle_next_command
    (queue : IRefValue<Runner_Queue>)
    (scenes : Scene_Map)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (queue_data : Runner_Queue_State_Loading_Data)
    (next_command_id : int<command_id>)
    : unit =

(* Get the scene for the next command. *)
        match scenes.TryFind queue_data.next_command_data.next_command_scene_id with

        | None -> error "add_commands_to_queue" "Next command scene ID not found." ["next_command_scene_id", queue_data.next_command_data.next_command_scene_id; "scenes", scenes] |> invalidOp

        | Some scene ->

(* Get the next command from its scene using the command ID. *)
            match scene.TryFind next_command_id with

            | None -> error "add_commands_to_queue" "Next command ID not found in current scene." ["scene", queue_data.next_command_data.next_command_scene_id; "next_command_id", next_command_id] |> invalidOp

            | Some command ->
                let command_data = get_command_data queue_data.next_command_data.next_command_scene_id runner_component_interfaces command queue_data.menu_variables
                let queue_data_2 = add_command_to_queue queue_data command_data
                match command_data.behavior with

(* We keep adding commands to the queue until we reach a command with behavior Wait_For_Callback. *)
                | Continue_Immediately _ ->
                    do queue.current <- Queue_Loading queue_data_2
                    add_commands_to_queue queue scenes runner_component_interfaces

(* Once we reach a command with behavior Wait_For_Callback, we do not add any more commands to the queue for now, and we set continue_after_finished and add_to_history to reflect the last command. *)
                | Wait_For_Callback wait ->
                    do queue.current <-
                        Queue_Running {
                            commands = queue_data_2.commands
                            next_command_data = queue_data_2.next_command_data
                            components_used_by_commands = queue_data_2.components_used_by_commands
                            continue_after_finished = wait.continue_afterward
                            add_to_history = wait.add_to_history
                            autosave = queue_data_2.autosave || wait.autosave
                            menu_variables = queue_data_2.menu_variables
                        }

and private add_commands_to_queue
    (queue : IRefValue<Runner_Queue>)
    (scenes : Scene_Map)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : unit =

(* Change the queue state to Queue_Loading to protect it from other functions. *)
    let queue_data =
        match queue.current with
        
        | Queue_Idle data ->
            {
                commands = Map.empty
                next_command_data = data.next_command_data
                components_used_by_commands = Set.empty
(* A queue state of Queue_Idle means this is the first call to add_commands_to_queue (), not a recursive one, so we need to clear the autosave flag. *)
                autosave = false
                menu_variables = data.menu_variables
            }

(* This function is recursive, so the queue state might already be Queue_Loading. *)
        | Queue_Loading data -> data

        | _ -> error "add_commands_to_queue" "Unexpected queue state." ["Queue state", queue.current] |> invalidOp

    let queue_state = Queue_Loading queue_data
    do queue.current <- queue_state

    #if debug
    do debug "add_commands_to_queue" String.Empty ["next_command_id", queue_data.next_command_data.next_command_id]
    #endif

(* Get the next command ID. *)
    match queue_data.next_command_data.next_command_id with
(* If there are no more commands to add to the queue... *)
    | None -> handle_no_more_commands queue queue_data
    | Some next_command_id_2 -> handle_next_command queue scenes runner_component_interfaces queue_data next_command_id_2

let private run_commands
    (queue : IRefValue<Runner_Queue>)
    (reason : Run_Reason)
    : unit =

    #if debug
    do debug "run_commands" String.Empty ["reason", reason; "queue", queue.current]
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
                        | Continue_Immediately _ -> None
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
    (queue : IRefValue<Runner_Queue>)
    (scenes : Scene_Map)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
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
            add_commands_to_queue queue scenes runner_component_interfaces
(* If there are no more commands, add_commands_to_queue sets the queue state to Queue_Done. Otherwise, it sets it to Queue_Running. *)
            match queue.current with
            | Queue_Running _ -> run_commands queue reason
            | _ -> ()
    | Queue_Loading _ -> do error "run" "Unexpected queue state (Queue_Loading)." [] |> invalidOp
    | Queue_Running _ ->
        do force_complete_transitions runner_component_interfaces queue false
            (fun () ->
                #if debug
                do debug "run" "Runner_State.force_complete_transitions () done." []
                #endif
                ()
            )
    | Queue_Interrupting _ -> do warn "run" false "Unexpected queue state (Queue_Interrupting)." []
(* If there are no more commands, ignore. *)
    | Queue_Done -> ()
