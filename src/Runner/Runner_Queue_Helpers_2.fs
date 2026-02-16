module Runner_Queue_Helpers_2

// String
open System

// console, window
open Browser.Dom
// IRefValue
open Fable.React

open Character_Types
open Command_Types
open JavaScript_Interop_1
open Image_Map
open Log
open Menu
open Parser_2
open Runner_Queue_Helpers_1
open Runner_Types
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Queue_Helpers_2"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Helper functions *)

let private handle_eval_command
    (scenes : IRefValue<Scene_Map>)
(* This function can change both scene and next_command_id if we get a Jump command. *)
    (next_command_1 : Next_Command_Data option)
    (menu_variables : Menu_Variables)
    (parser : Parser)
    (eval_content_1 : string)
    : Next_Command_Data option =

(* Evaluate any JavaScript interpolations in the eval command content. *)
    let eval_content_2 = eval_js_with_menu_variables<string> eval_content_1 menu_variables

(* Create a temporary script to contain the eval command content, and parse it. *)
    let next_command_scene_id = temporary_scene_id_for_eval_command
    let script = {
(* Use a temporary scene ID that does not conflict with existing scene IDs. *)
        id = next_command_scene_id
        name = "Temporary scene for eval command"
        content = eval_content_2
    }
(* If the parse fails, it raises a Parsing_Semantics_Error, which is caught by Parser_1_Semantics.parse_script_1 () and re-raised as a generic exception. From here, it is caught by Runner_Queue.handle_next_command (). *)
    let temporary_scene_map = get_scene_map parser [script]
    let temporary_scene = temporary_scene_map |> Seq.head

(* Get the last command in the temporary scene. We want to append a Jump command that takes the player back to the command after the Eval command in the current scene, unless the Eval command is the last command in the current scene.

TODO2 #parsing We could just add a Jump command to the temporary script, but we would need a special undocumented form of Jump that accepts a target command ID as well as a target scene ID.
*)
    let last_command_1 = temporary_scene.Value.commands |> Seq.last
    let last_command_id, last_command_2 = last_command_1.Key, last_command_1.Value

    let temporary_scene_commands =
        match next_command_1 with
(* If the Eval command is the last command in the current scene, do not append the Jump command to the temporary scene. *)
        | None -> temporary_scene.Value.commands
    
        | Some next_command_2 ->

            let jump_command_id = last_command_id + 1<command_id>
            let jump_command =
                {
                    id = jump_command_id
                    next_command_id = None
                    parent_command_id = None
                    command = Jump { scene_id = next_command_2.next_command_scene_id; command_id = next_command_2.next_command_id } |> Command
(* This should never be used. *)
                    error_data = {
                        source = $"jump {next_command_2.next_command_scene_id}"
                        scene_id = next_command_2.next_command_scene_id
                        script_text_index = eval_content_2.Length
                    }
                }
(* Modify the last command so its next command is the Jump command. *)
            let last_command_3 = {
                last_command_2 with
                    next_command_id = Some jump_command_id
            }
(* Insert the modified last command into the temporary scene. *)
            let commands_1 = temporary_scene.Value.commands.Add (last_command_id, last_command_3)
(* Insert the jump command into the temporary scene. *)
            commands_1.Add (jump_command_id, jump_command)

(* Update the temporary scene with the updated commands. Insert the temporary scene into the scene map. *)
    do scenes.current <- scenes.current.Add (temporary_scene.Key, { temporary_scene.Value with commands = temporary_scene_commands })

(* If the author calls Eval with no content, the parse will fail, so None is never a valid return value. *)
    Some {
        next_command_scene_id = next_command_scene_id
        next_command_id = last_command_id
    }

let private handle_command 
    (runner_state : Runner_State)
(* This function can change both scene and next_command_id if we get a Jump command. *)
    (next_command : Next_Command_Data option)
    (command_1 : Command_Type)
    (error_data : Command_Error_Data_2)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    let behavior = command_to_behavior command_1
    let component_ids = command_to_component_ids command_1

    let command_2 : Runner_Command_Signature option =
        match command_1 with

        | Music_Play command_2 ->
            Some <| fun _ -> do runner_state.runner_component_interfaces.current.music.current.play command_2

        | Music_Stop ->
            Some <| fun _ -> do runner_state.runner_component_interfaces.current.music.current.stop ()

        | Temporary_Notification command_2 ->
            Some <| fun _ ->
                do runner_state.runner_component_interfaces.current.notifications.current.add_temporary_notification { text = eval_js_with_menu_variables<string> command_2.text menu_variables }

        | Permanent_Notification command_2 ->
            Some <| fun _ ->
                do runner_state.runner_component_interfaces.current.notifications.current.set_permanent_notification command_2.text menu_variables

        | Hide_Permanent_Notification ->
            Some <| fun _ -> do runner_state.runner_component_interfaces.current.notifications.current.hide_permanent_notification ()

        | Background_Fade_In command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.background.current.fade_in command_2.new_url command_2.transition_time command_queue_item_id

        | Background_Fade_Out command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.background.current.fade_out command_2.transition_time command_queue_item_id

        | Background_Cross_Fade command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.background.current.cross_fade command_2.new_url command_2.transition_time command_queue_item_id

        | Character_Fade_In command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.characters.current.fade_in command_2.character_short_name command_2.url command_2.position command_2.transition_time command_queue_item_id

        | Character_Fade_Out command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.characters.current.fade_out command_2.character_short_name command_2.transition_time command_queue_item_id

        | Character_Cross_Fade command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.characters.current.cross_fade command_2.character_short_name command_2.url command_2.transition_time command_queue_item_id

        | Character_Move command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                match command_2 with
                | In data ->
                    do runner_state.runner_component_interfaces.current.characters.current.move_in data.character_short_name data.url data.direction data.position data.transition_time command_queue_item_id
                | Out data ->
                    do runner_state.runner_component_interfaces.current.characters.current.move_out data.character_short_name data.direction data.transition_time command_queue_item_id

        | Fade_Out_All transition_time ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do
                    runner_state.runner_component_interfaces.current.dialogue_box.current.hide true <| Some command_queue_item_id
                    runner_state.runner_component_interfaces.current.characters.current.fade_out_all transition_time command_queue_item_id
                    runner_state.runner_component_interfaces.current.background.current.fade_out transition_time command_queue_item_id

        | Dialogue_Box_Show ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.dialogue_box.current.show true <| Some command_queue_item_id

        | Dialogue_Box_Hide ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.dialogue_box.current.hide true <| Some command_queue_item_id

        | Dialogue command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.dialogue_box.current.type_dialogue command_2.character_full_name (eval_js_with_menu_variables<string> command_2.text menu_variables) command_queue_item_id

        | JavaScript_Inline command_2 ->
            Some <| fun _ ->
                do
                    eval_js_with_menu_variables<unit> command_2.code menu_variables
                    runner_state.runner_component_interfaces.current.notifications.current.update_permanent_notification menu_variables

        | JavaScript_Block command_2 ->
            Some <| fun _ ->
                do
                    eval_js_with_menu_variables<unit> command_2.code menu_variables
                    runner_state.runner_component_interfaces.current.notifications.current.update_permanent_notification menu_variables

(* Jump and Eval are special cases that change the next scene and/or command. *)
        | Jump _ -> None

        | Eval _ -> None

    let next_command_2 =
        match command_1 with

        | Jump jump_data ->
(* We verify the jump destination in Parser_1_Match_Patterns.match_jump (). *)
            Some {
                next_command_scene_id = jump_data.scene_id;
                next_command_id = jump_data.command_id
            }

        | Eval eval_data ->
            handle_eval_command runner_state.scenes next_command menu_variables runner_state.parser eval_data.eval_content

        | _ -> next_command

    {
        command = command_2
        error_data = error_data
        behavior = behavior
        components_used = component_ids
        next_command = next_command_2
    }

(* Like Jump and Eval, If is a special case that changes the next command, though not the next scene. *)
let private handle_if_2
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
            next_command = Some {
                next_command_2 with
                    next_command_id = next_command_id_1
            }
        }

let private handle_if_1
    (scenes : Scene_Map)
    (next_command : Next_Command_Data option)
    (command : If_Block)
    (error_data : Command_Error_Data_2)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    let known_error_data_1 = ["source", error_data.source :> obj]

// TODO1 #javascript We could remove this. We now handle this in Runner_Queue.handle_next_command () so we can also catch errors in Eval there.
(* handle_if_1 () does not return a command that is run by Runner_Queue.run_command (), so we must handle JavaScript errors here.
get_command_data () also does not return a command for End_If, but End_If does not allow JavaScript, so there is no issue there.
*)
    try handle_if_2 next_command command error_data menu_variables with
    | Run_Time_JavaScript_Error e ->
        let known_error_data_2 = known_error_data_1 @ ["code", e.code; "message", e.inner.Message]
        let script_name, script_line_number = get_script_name_and_line_number scenes "handle_if_1" error_data known_error_data_2
        error "handle_if_1" "JavaScript error." (known_error_data_2 @ ["script_name", script_name; "script_line_number", script_line_number]) |> invalidOp
    | _ -> reraise ()

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
        next_command = next_command
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
        next_command = next_command
    }

(* Main functions *)

let get_command_data
    (runner_state : Runner_State)
    (next_command_scene_id : int<scene_id>)
    (command : Command_Post_Parse)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    let next_command =
        match command.next_command_id with
        | None -> None
        | Some next_command_id_2 ->
            Some {
(* By default, the next command belongs to the same scene. *)
                next_command_scene_id = next_command_scene_id
                next_command_id = next_command_id_2
            }

    match command.command with

    | Command_Post_Parse_Type.Command command_1 -> handle_command runner_state next_command command_1 command.error_data menu_variables

    | Command_Post_Parse_Type.If command_2 -> handle_if_1 runner_state.scenes.current next_command command_2 command.error_data menu_variables

    | Command_Post_Parse_Type.End_If ->
        {
            command = None
            error_data = command.error_data
            behavior = end_if_behavior
            components_used = Set.empty
            next_command = next_command
        }

    | Command_Post_Parse_Type.Menu command_3 -> handle_menu runner_state.runner_component_interfaces next_command command_3 command.error_data menu_variables

    | Command_Post_Parse_Type.Image_Map command_4 -> handle_image_map runner_state.runner_component_interfaces next_command command_4 command.error_data menu_variables

    | Command_Post_Parse_Type.End_Image_Map transition_time ->
        {
            command = Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.image_map.current.fade_out transition_time command_queue_item_id
            error_data = command.error_data
            behavior = end_image_map_behavior
            components_used = Set.singleton Runner_Component_Names.Image_Map
            next_command = next_command
        }

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

