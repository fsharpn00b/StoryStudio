module Runner_Queue_Helpers_3

// String
open System

// console, window
open Browser.Dom
// IRefValue
open Fable.React

open Command_Types
open JavaScript_Interop_1
open Image_Map
open Log
open Menu
open Runner_Queue_Helpers_1
open Runner_Queue_Helpers_2
open Runner_Types_2
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Queue_Helpers_3"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Functions - helpers *)

(* Like Jump and Eval, If is a special case that changes the next command, though not the next scene. *)
let private handle_if
    (next_command_1 : Next_Command_Data option)
    (command : If_Block)
    (error_data : Command_Error_Data_2)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    match next_command_1 with
    | None -> error "handle_if_2" "If block does not have a next command. It cannot be the last command." ["if_block", command] |> invalidOp
    | Some next_command_2 ->

        let next_command_id_1 : int<command_id> =
(* If the If conditional is true, the next command ID is the If child command ID. *)
            if eval_js_with_menu_variables<bool> command.conditional menu_variables then
                command.child_command_id
            else
(* Otherwise, see if any of the Else_If conditionals are true. The next command ID is the child command ID of the first Else_If whose conditional is true. *)
                let next_command_id_2 =
                    command.else_if_blocks |> List.tryPick (fun block ->
                        if eval_js_with_menu_variables<bool> block.conditional menu_variables then
                            Some block.child_command_id
                        else None
                )
                match next_command_id_2 with
                | Some next_command_id_3 -> next_command_id_3
                | None ->
(* If none of the Else_If conditionals are true, see if we have an Else. *)
                    match command.else_block with
                    | Some next_command_id_4 -> next_command_id_4
(* If we have no Else, the next command ID is the If next command ID. *)
                    | None -> next_command_2.next_command_id

        {
            command = None
            error_data = error_data
            behavior = if_behavior
            components_used = Set.empty
            next_command_data = Some {
                next_command_2 with
                    next_command_id = next_command_id_1
            }
        }

let private handle_menu
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (next_command : Next_Command_Data option)
    (menu_data_1 : Menu_Data_1)
    (error_data : Command_Error_Data_2)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

(* TODO2 Consider using lazy everywhere we use fun () -> (delay). What is the difference if any? *)
    let command = fun (command_queue_item_id : int<command_queue_item_id>) ->

        let menu_data_2 = {
            name = menu_data_1.name
            text = eval_js_with_menu_variables<string> menu_data_1.text menu_variables
            items = menu_data_1.items |> List.choose (fun item_1 ->
(* TODO2 We do not remember why we needed to delay eval of menu item text. Presumably because otherwise we evaluate the JavaScript too soon. *)
                let item_2 = lazy {
                    value = item_1.value
                    text = eval_js_with_menu_variables<string> item_1.text menu_variables
                }
                match item_1.conditional with
                    | Some conditional ->
                        if eval_js_with_menu_variables<bool> conditional menu_variables then Some item_2.Value
                        else None
                    | None -> Some item_2.Value
            )
        }
        do runner_component_interfaces.current.menu.current.show menu_data_2 true (Some command_queue_item_id)

    {
        command = Some command
        error_data = error_data
        behavior = menu_behavior
        components_used = Set.singleton Menu
        next_command_data = next_command
    }

let private handle_image_map
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (next_command : Next_Command_Data option)
    (image_map_data_1 : Image_Map_Data)
    (error_data : Command_Error_Data_2)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    let command = fun (command_queue_item_id : int<command_queue_item_id>) ->

        let image_map_data_2 = {
            image_map_data_1 with
                items = image_map_data_1.items |> List.filter (fun item ->
                    match item.conditional with
                    | Some conditional -> eval_js_with_menu_variables<bool> conditional menu_variables
                    | None -> true
                )
        }
        do runner_component_interfaces.current.image_map.current.fade_in image_map_data_2 image_map_data_2.transition_time command_queue_item_id

    {
        command = Some command
        error_data = error_data
        behavior = image_map_behavior
        components_used = Set.singleton Image_Map
        next_command_data = next_command
    }

(* Functions - main *)

(* This function transforms a Command_Post_Parse into a Runner_Command_Data. *)
let get_runner_command_data
    (runner_state : Runner_State)
    (command : Command_Post_Parse)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    match command.command with

    | Command_Post_Parse_Type.Command command_1 -> handle_command runner_state command.next_command_data command_1 command.error_data menu_variables

    | Command_Post_Parse_Type.If command_2 -> handle_if command.next_command_data command_2 command.error_data menu_variables

    | Command_Post_Parse_Type.End_If ->
        {
            command = None
            error_data = command.error_data
            behavior = end_if_behavior
            components_used = Set.empty
            next_command_data = command.next_command_data
        }

    | Command_Post_Parse_Type.Menu command_3 -> handle_menu runner_state.runner_component_interfaces command.next_command_data command_3 command.error_data menu_variables

    | Command_Post_Parse_Type.Image_Map command_4 -> handle_image_map runner_state.runner_component_interfaces command.next_command_data command_4 command.error_data menu_variables

    | Command_Post_Parse_Type.End_Image_Map transition_time ->
        {
            command = Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.image_map.current.fade_out transition_time command_queue_item_id
            error_data = command.error_data
            behavior = end_image_map_behavior
            components_used = Set.singleton Runner_Component_Names.Image_Map
            next_command_data = command.next_command_data
        }

    | Command_Post_Parse_Type.Jump_Scene ->
        {
            command = None
            error_data = command.error_data
            behavior = jump_scene_behavior
            components_used = Set.empty
            next_command_data = command.next_command_data
        }

    | Command_Post_Parse_Type.Jump_Label
    | Command_Post_Parse_Type.Jump_Internal
    | Command_Post_Parse_Type.Label ->
        {
            command = None
            error_data = command.error_data
            behavior = label_and_jump_label_behavior
            components_used = Set.empty
            next_command_data = command.next_command_data
        }

(* TODO2 #parsing In Runner_Queue_Helpers_2, for some commands (If, Jump, Eval), we essentially run these commands immediately, instead of delaying them and adding them to the queue. These commands have a special function, to change the next command we run. We must run them immediately because otherwise we might add commands that follow them to the queue when we should not. For instance, if we have commands:

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

x We need to see if this issue arises for any other command that can affect JS state.
x menu
x image_map
x What else? We looked at all calls to JavaScript_Interop_1.eval_js_with_menu_variables () but couldn't find any.

Runner_Notify.get_notify_menu_selection () and get_notify_image_map_selection () both set the queue state to Queue_Idle and update menu_variables. So it should not be possible to run a command after menu or image_map that depends on changes they make to JS state but runs before those changes can be recorded.

N Just get rid of the queue, or reduce it to running one command at a time, as soon as we encounter it. We tried this, but it was very painful, and it would be even more painful if we needed to re-add the queue at some point, such as if we want to run multiple commands in parallel.
N Keep the Wait_For_Callback/Continue_Immediately command behaviors, but only so we know whether the queue should run the command and then exit and wait for the callback, or recursively call run ().
*)
