module Runner_Queue

// String
open System

// console, window
open Browser.Dom
// IRefValue
open Feliz

open Command_Types
open JavaScript_Interop_1
open Log
open Runner_Queue_Helpers_1
open Runner_Queue_Helpers_2
open Runner_Transition
open Runner_Types
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Queue"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Functions - helpers *)

let private run_command
    (scenes : IRefValue<Scene_Map>)
    (error_data : Command_Error_Data_2)
    (f : unit -> unit)
    : unit =

    let known_error_data_1 = ["source", error_data.source :> obj]

(* Note We also need to catch JavaScript errors in handle_next_command (). For If and Eval commands, we can try to evaluate JavaScript outside the delayed function f (). *)
    try f () with
    | Run_Time_JavaScript_Error e ->
        let known_error_data_2 = known_error_data_1 @ ["code", e.code; "message", e.inner.Message]
        let script_name, script_line_number = get_script_name_and_line_number scenes.current "run_command" error_data known_error_data_2
        error "run_command" "JavaScript error." (known_error_data_2 @ ["script_name", script_name; "script_line_number", script_line_number]) |> invalidOp
    | e ->
(* We should not see a generic exception here. If we catch an error raised by Log.error (), this presumably shows the user two alerts. However, we do want to add the script name and line number for the command in question. *)
        let known_error_data_2 = known_error_data_1 @ ["message", e.Message]
        let script_name, script_line_number = get_script_name_and_line_number scenes.current "run_command" error_data known_error_data_2
        error "run_command" "Error running command." (known_error_data_2 @ ["script_name", script_name; "script_line_number", script_line_number]) |> invalidOp

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
            next_command_queue_item_id = queue_data.next_command_data.next_command_queue_item_id + 1<command_queue_item_id>
            next_command = command_data.next_command
        }
        components_used_by_commands =
            if Set.empty <> Set.intersect queue_data.components_used_by_commands command_data.components_used then
                do warn "add_command_to_queue" false "Overlap between components used by command and components used by commands already in queue." ["Components used by commands already in queue", queue_data.components_used_by_commands; "command", command_data.error_data.source; "Components used by command", command_data.components_used]
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
    (scenes : IRefValue<Scene_Map>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (queue_data : Runner_Queue_State_Loading_Data)
    (next_command : Next_Command_Data)
    (parser : Parser)
    : unit =

(* Get the scene for the next command. *)
    match scenes.current.TryFind next_command.next_command_scene_id with

    | None -> error "add_commands_to_queue" "Next command scene ID not found." ["next_command_scene_id", next_command.next_command_scene_id; "scenes", scenes] |> invalidOp

    | Some scene ->

(* Get the next command from its scene using the command ID. *)
        match scene.commands.TryFind next_command.next_command_id with

        | None -> error "add_commands_to_queue" "Next command ID not found in current scene." ["scene", next_command.next_command_scene_id; "next_command_id", next_command.next_command_id] |> invalidOp

        | Some command ->
            let command_data =
(* Runner_Queue_Helpers_2.get_command_data () can try to evaluate JavaScript for If or Eval commands. *)
                try get_command_data runner_component_interfaces scenes next_command.next_command_scene_id command queue_data.menu_variables parser with
                | Run_Time_JavaScript_Error e ->
                    let known_error_data = ["code", e.code :> obj; "message", e.inner.Message]
                    let script_name, script_line_number = get_script_name_and_line_number scenes.current "handle_next_command" command.error_data known_error_data
                    error "handle_next_command" "JavaScript error." (known_error_data @ ["script_name", script_name; "script_line_number", script_line_number]) |> invalidOp
(* Unlike with run_command, we might see a generic exception here. Runner_Queue_Helpers_2.handle_eval () and handle_if_2 () can raise one. *)
                | e ->
                    let known_error_data = ["message", e.Message :> obj]
                    let script_name, script_line_number = get_script_name_and_line_number scenes.current "handle_next_command" command.error_data known_error_data
                    error "handle_next_command" "Error running command." (known_error_data @ ["script_name", script_name; "script_line_number", script_line_number]) |> invalidOp

            let queue_data_2 = add_command_to_queue queue_data command_data
            match command_data.behavior with

(* We keep adding commands to the queue until we reach a command with behavior Continue_Immediately/run_queue_now = true or Wait_For_Callback. *)
            | Continue_Immediately behavior ->
                if behavior.run_queue_now then
                    do queue.current <-
                        Queue_Running {
                            commands = queue_data_2.commands
                            next_command_data = queue_data_2.next_command_data
                            components_used_by_commands = queue_data_2.components_used_by_commands
                            continue_after_finished = true
                            add_to_history = false
                            autosave = queue_data_2.autosave || behavior.autosave
                            menu_variables = queue_data_2.menu_variables
                        } 
                else
                    do queue.current <- Queue_Loading queue_data_2
                    add_commands_to_queue queue scenes runner_component_interfaces parser

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

(* TODO1 #parsing Create a Runner_State that contains
- scenes : IRefValue<Scene_Map>
- queue : IRefValue<Runner_Queue>
- runner_component_interfaces : IRefValue<Runner_Component_Interfaces>
- parser : Parser

Update the scene map and queue as we go.

- Where is the other mutable state we have like this?
*)
(* Once we reach a command with behavior Wait_For_Callback, we do not add any more commands to the queue for now, and we set continue_after_finished and add_to_history to reflect the last command. *)

and private add_commands_to_queue
    (queue : IRefValue<Runner_Queue>)
    (scenes : IRefValue<Scene_Map>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (parser : Parser)
    : unit =

(* Change the queue state to Queue_Loading to protect it from other functions. *)
    let queue_data =
        match queue.current with
        
        | Queue_Idle data ->
            {
                commands = Map.empty
                next_command_data = data.next_command
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
    match queue_data.next_command_data.next_command with
(* If there are no more commands to add to the queue... *)
    | None -> handle_no_more_commands queue queue_data
    | Some next_command_2 -> handle_next_command queue scenes runner_component_interfaces queue_data next_command_2 parser

(* Functions - main *)

let rec private run_commands
    (queue : IRefValue<Runner_Queue>)
    (scenes : IRefValue<Scene_Map>)
    (reason : Run_Reason)
    (parser : Parser)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
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
                        let command_data = kv.Value.command_data
                        match command_data.command with
                        | Some f -> run_command scenes command_data.error_data (fun () -> f kv.Key)
                        | None -> ()

(* If the command does not wait for a transition to complete, remove it from the queue. *)
                        match kv.Value.command_data.behavior with
                        | Continue_Immediately _ -> None
                        | _ -> Some (kv.Key, kv.Value)
                    )
                    |> Map.ofSeq

(* TODO1 #parsing In Runner_Queue_Helpers_2, for some commands (If, Jump, Eval), we essentially run these commands immediately, instead of delaying them and adding them to the queue. These commands have a special function, to change the next command we run. We must run them immediately because otherwise we might add commands that follow them to the queue when we should not. For instance, if we have commands:

Jump <another scene>
fadeoutall

If we do not run Jump immediately, we will add both commands to the queue, but we do not want to run fadeoutall.

However, this can be a problem because If and Eval can evalulate JavaScript. If we have commands:

js window.state.x = true
if true === window.state.x

The If will raise an exception because x is undefined. That is because we add the JS command to the queue, but do not run it yet, and then we run the If command immediately, before we run the queue.

For now, we handle this with a new command behavior, Continue_Immediately/run_queue_now = true, which we apply to JS commands. When we encounter a command with this behavior, we stop adding commands to the queue and run the queue.

Remaining work:

- We could change Runner_Command_Signature to return Next_Command_Data, then change Runner_Queue_Helpers_2 to delay If, Jump, and Eval instead of running them immediately. However, for the reason mentioned previously, we would also need to change If, Jump and Eval to behavior Continue_Immediately/run_queue_now = true, to make sure we did not add any commands that follow them to the queue. We're not sure this would work for If.

- We need to see if issue arises for any other command that can affect JS state.
- menu
- image_map
- What else?

We think menu and image_map stop and run the queue, though, so the issue should not arise.

- Just get rid of the queue, or reduce it to running one command at a time, as soon as we encounter it.
- Keep the Wait_For_Callback/Continue_Immediately command behaviors, but only so we know whether the queue should run the command and then exit and wait for the callback, or recursively call run ().
*)

(* If there are no commands remaining, we know that both:

1 The queue only contained commands with behavior Continue_Immediately.

2 Either:
2a We are done, but we let run (), add_commands_to_queue (), and handle_no_more_commands () determine that.
2b The last command in the queue had behavior Continue_Immediately/run_queue_now = true, so we might not be done.

When we encounter a command with behavior Wait_For_Callback, we stop adding commands to the queue and run the commands already in the queue. We do not remove that command from the queue after running it.

Previously, case 2b did not exist. If there were no commands remaining, it meant we had reached the end of the scene, and the last command in the scene had behavior Continue_Immediately. If we reach the end of a scene, we stop. If the author wants to continue to another scene, they must use a Jump command. So we set the queue state to Queue_Done.

Now, however, we also stop adding commands to the queue when we encounter a command with behavior Continue_Immediately/run_queue_now = true. However, after we run this command, we must call run () again manually.

We also transition from Queue_Running to Queue_Idle in Runner_Queue_Transition.remove_transition_1 () and remove_transition_2 ().
*)
            if Map.isEmpty commands then
                do
                    queue.current <- Queue_Idle {
                        next_command = data.next_command_data
                        add_to_history = data.add_to_history
                        autosave = data.autosave
                        menu_variables = data.menu_variables
                    }
                    run queue scenes runner_component_interfaces reason parser
(* If there is a command remaining, it must be one that waits for a transition to complete. *)
            else do queue.current <- Queue_Running { data with commands = commands }

    | _ -> error "run_commands" "Unexpected queue state." ["queue", queue.current] |> invalidOp

(* Ideally, queue would be a value, not a reference. But it must be a reference so it can also be accessed by remove_transition (), which is called by Runner_Transition.notify_transition_complete (), which in turn is called by various components. *)
and run
    (queue : IRefValue<Runner_Queue>)
    (scenes : IRefValue<Scene_Map>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
(* Previously, this was run_manually : bool. We used it to determine whether we were running a command automatically, or running (or interrupting) a command at player request. Now that is handled by queue.state. reason is for debugging.
*)
    (reason : Run_Reason)
    (parser : Parser)
    : unit =

    #if debug
    do debug "run" String.Empty <| ["reason", reason; "queue", queue.current]
    #endif

    match queue.current with
    | Queue_Idle _ ->
        do
            add_commands_to_queue queue scenes runner_component_interfaces parser
(* If there are no more commands, add_commands_to_queue sets the queue state to Queue_Done. Otherwise, it sets it to Queue_Running. *)
            match queue.current with
            | Queue_Running _ -> run_commands queue scenes reason parser runner_component_interfaces
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
