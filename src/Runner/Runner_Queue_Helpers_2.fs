module Runner_Queue_Helpers_2

// Environment.NewLine
open System

// console, window
open Browser.Dom
// IRefValue
open Fable.React

open Command_Types
open JavaScript_Interop_1
open Log
open Parser_2_2
open Runner_Queue_Helpers_1
open Runner_Types_2
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Queue_Helpers_2"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Helper functions *)

let handle_eval_command_1
    (scenes : IRefValue<Scene_Map>)
    (next_command_1 : Next_Command_Data option)
    (menu_variables : Menu_Variables)
    (parser : Parser)
    (eval_content_1 : string)
    : Next_Command_Data option =

(* To handle Eval, we:
1 Create a temporary script to contain the Eval command content.
2 Append a jump command to the temporary script that takes the player back to the command following the Eval command in the current scene.
3 Parse the temporary script into a scene.
4 Redirect the next_command_data for the Eval command to point to the first command in the temporary scene.
(end)
*)

(* Evaluate any JavaScript interpolations in the eval command content. *)
    let eval_content_2 = eval_js_with_menu_variables<string> eval_content_1 menu_variables

    let eval_content_3 =
        match next_command_1 with

(* If the Eval command is the last command in the current scene, we do not need to return after the temporary scene. *)
        | None -> eval_content_2

        | Some next_command_2 ->
(* Add a jump command to the end of the temporary scene to return to the current scene. *)
            $"{eval_content_2}{Environment.NewLine}jump {next_command_2.next_command_scene_id} {next_command_2.next_command_id}"

    let script = {
(* Use a temporary scene ID that does not conflict with existing scene IDs. *)
        id = temporary_scene_id_for_eval_command
        name = "Temporary scene for eval command"
        content = eval_content_3
    }
(* If the parse fails, it raises a Parsing_Semantics_Error, which is caught by Parser_1_Semantics.parse_script_1 () and re-raised as a generic exception. From here, it is caught by Runner_Queue.handle_next_command (). *)
    let temporary_scene_map = get_scene_map parser [script]
    let temporary_scene = temporary_scene_map |> Seq.head

(* Insert the temporary scene into the scene map. *)
    do scenes.current <- scenes.current.Add (temporary_scene.Key, temporary_scene.Value)
(* If the author calls Eval with no content, the parse will fail, so None is never a valid return value. *)
    Some {
        next_command_scene_id = temporary_scene_id_for_eval_command
        next_command_id = scene_initial_command_id
    }

let handle_command 
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

        | Character_Transition command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                do runner_state.runner_component_interfaces.current.characters.current.transition command_2 command_queue_item_id

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

(* Eval, Jump_Scene, and Jump_Label are special cases that change the next scene and/or command. We handle Jump_Scene and Jump_Label while parsing. We cannot handle Eval until runtime because we do not know exactly what it contains until we apply JavaScript interpolations, which depend on the runtime state. *)
        | Eval _ -> None

    let next_command_2 =
        match command_1 with

        | Eval eval_data ->
            handle_eval_command_1 runner_state.scenes next_command menu_variables runner_state.parser eval_data.eval_content

        | _ -> next_command

    {
        command = command_2
        error_data = error_data
        behavior = behavior
        components_used = component_ids
        next_command_data = next_command_2
    }
