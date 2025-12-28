module Runner_Queue_Helpers

// DateTime, String
open System

// IRefValue
open Fable.React

open Character_Types
open Command_Types
open JavaScript_Interop
open Image_Map
open Log
open Menu
open Runner_Types
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Queue_Helpers"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

let private if_behavior = Continue_Immediately { autosave = false }
let private end_if_behavior = Continue_Immediately { autosave = false }
let private menu_behavior = Wait_For_Callback { continue_afterward = false; add_to_history = true; autosave = true }
let private image_map_behavior = Wait_For_Callback { continue_afterward = false; add_to_history = true; autosave = true }
let private end_image_map_behavior = Wait_For_Callback { continue_afterward = true; add_to_history = false; autosave = false }

(* Helper functions *)

let private command_to_behavior (command : Command) : Command_Behavior =
    match command with
    | Background_Fade_In _
    | Background_Fade_Out _
    | Background_Cross_Fade _
    | Character_Fade_In _
    | Character_Fade_Out _
    | Character_Cross_Fade _
    | Fade_Out_All _
    | Dialogue_Box_Show
    | Dialogue_Box_Hide -> Wait_For_Callback { continue_afterward = true; add_to_history = false; autosave = false }
(* Pause and wait for the user to continue.

We set continue_after_running to false because, even if the dialogue box has its typing speed is 0 (meaning text is shown at once and in full), we let it signal that the transition is complete, to simplify the logic here.
*)
    | Dialogue _ -> Wait_For_Callback { continue_afterward = false; add_to_history = true; autosave = false }
    | Music_Play _
    | Music_Stop
    | Temporary_Notification _
// TODO2 Possibly these should not be commands, or should at least be a separate subtype.
    | JavaScript_Inline _
    | JavaScript_Block _ -> Continue_Immediately { autosave = false }
    | Jump _ -> Continue_Immediately { autosave = true }

let private command_to_component_ids (command : Command) : Runner_Component_Names Set =
    match command with
    | Background_Fade_In _
    | Background_Fade_Out _
    | Background_Cross_Fade _ -> Set.singleton Background
    | Character_Fade_In _
    | Character_Fade_Out _
    | Character_Cross_Fade _ -> Set.singleton Characters
    | Fade_Out_All _ -> Set.ofList [Background; Characters; Dialogue_Box]
    | Dialogue_Box_Show
    | Dialogue_Box_Hide
    | Dialogue _ -> Set.singleton Dialogue_Box
(* The Music component does not have transitions. *)
    | Music_Play _
    | Music_Stop
(* The Notifications component does not notify Runner when it completes a transition. *)
    | Temporary_Notification _
    | JavaScript_Inline _
    | JavaScript_Block _
    | Jump _ -> Set.empty

let private handle_command 
    (runner_components : IRefValue<Runner_Components>)
(* This function can change both scene and next_command_id if we get a Jump command. *)
    (current_scene_id : int<scene_id>)
    (next_command_id_1 : int<command_id> option)
    (command_1 : Command)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    let behavior = command_to_behavior command_1
    let component_ids = command_to_component_ids command_1

    let command_2 =

        match command_1 with

        | Music_Play command_2 ->
            Some <| fun _ -> do runner_components.current.music.current.play command_2

        | Music_Stop ->
            Some <| fun _ -> do runner_components.current.music.current.stop ()

        | Temporary_Notification command_2 ->
// TODO1 There is no way to see whether we have applied JS interpolation to the text field yet. Use separate types? This applies to other commands that use JS interpolation also.
            Some <| fun _ -> do runner_components.current.notifications.current.add_temporary_notification { command_2 with text = eval_js_string_with_menu_variables command_2.text menu_variables }

        | Background_Fade_In command_2 ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> do runner_components.current.background.current.fade_in command_2.new_url command_2.transition_time command_queue_item_id

        | Background_Fade_Out command_2 ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> do runner_components.current.background.current.fade_out command_2.transition_time command_queue_item_id

        | Background_Cross_Fade command_2 ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> do runner_components.current.background.current.cross_fade command_2.new_url command_2.transition_time command_queue_item_id

        | Character_Fade_In command_2 ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> do runner_components.current.characters.current.fade_in command_2.character_short_name command_2.url command_2.position command_2.transition_time command_queue_item_id

        | Character_Fade_Out command_2 ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> runner_components.current.characters.current.fade_out command_2.character_short_name command_2.transition_time command_queue_item_id

        | Character_Cross_Fade command_2 ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> runner_components.current.characters.current.cross_fade command_2.character_short_name command_2.url command_2.transition_time command_queue_item_id

        | Fade_Out_All transition_time ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) ->
                runner_components.current.dialogue_box.current.hide true <| Some command_queue_item_id
                runner_components.current.characters.current.fade_out_all transition_time command_queue_item_id
                runner_components.current.background.current.fade_out transition_time command_queue_item_id
                
        | Dialogue_Box_Show ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> runner_components.current.dialogue_box.current.show true <| Some command_queue_item_id

        | Dialogue_Box_Hide ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> runner_components.current.dialogue_box.current.hide true <| Some command_queue_item_id

        | Dialogue command_2 ->
            Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> runner_components.current.dialogue_box.current.type_dialogue command_2.character_full_name (eval_js_string_with_menu_variables command_2.text menu_variables) command_queue_item_id

        | JavaScript_Inline command_2 ->
            Some <| fun _ -> eval_js_with_menu_variables command_2 menu_variables |> ignore

        | JavaScript_Block command_2 ->
            Some <| fun _ -> eval_js_with_menu_variables command_2 menu_variables |> ignore

        | Jump _ -> None

(* Jump is a special case that changes the next scene and command. *)
    let next_command_scene_id, next_command_id_2 =
        match command_1 with
        | Jump scene_id ->
(* We verify the jump destination in Parser_1_Match_Patterns.match_jump (). *)
            scene_id, Some scene_initial_command_id
        | _ -> current_scene_id, next_command_id_1

    {
        command = command_2
        debug_data = command_1.ToString ()
        behavior = behavior
        components_used = component_ids
        next_command_scene_id = next_command_scene_id
        next_command_id = next_command_id_2
    }

let private handle_if
    (current_scene_id : int<scene_id>)
    (next_command_id_1 : int<command_id> option)
    (command : If_Block)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    let next_command_id_2 : int<command_id> =
(* If the If conditional is true, the next command ID is the If child command ID. *)
        if eval_js_boolean_with_menu_variables command.conditional menu_variables then
            command.child_command_id
        else
(* Otherwise, see if any of the Else_If conditionals are true. The next command ID is the child command ID of the first Else_If whose conditional is true. *)
            let next_command_id_3 = command.else_if_blocks |> List.tryPick (fun block ->
                if eval_js_boolean_with_menu_variables block.conditional menu_variables then Some block.child_command_id else None
            )
            match next_command_id_3 with
            | Some next_command_id_4 -> next_command_id_4
            | None ->
(* If none of the Else_If conditionals are true, see if we have an Else. *)
                match command.else_block with
                | Some next_command_id_5 -> next_command_id_5
                | None ->
(* If we have no Else, the next command ID is the If next command ID. *)
                    match next_command_id_1 with
                    | Some next_command_id_6 -> next_command_id_6
                    | None -> error "handle_if" "If block does not have a next_command_id." ["if_block", command; "current_scene", current_scene_id] |> invalidOp

    {
        command = None
        debug_data = command.ToString ()
        behavior = if_behavior
        components_used = Set.empty
        next_command_scene_id = current_scene_id
        next_command_id = Some next_command_id_2
    }

let private handle_menu
    (runner_components : IRefValue<Runner_Components>)
    (current_scene_id : int<scene_id>)
    (next_command_id : int<command_id> option)
    (menu_data_1 : Menu_Data)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

(* TODO2 Consider using lazy everywhere we use fun () -> (delay). What is the difference if any? *)
    let command = fun (command_queue_item_id : int<runner_queue_item_id>) ->

        let menu_data_2 = {
            menu_data_1 with
                description = eval_js_string_with_menu_variables menu_data_1.description menu_variables
                items = menu_data_1.items |> List.choose (fun item_1 ->
(* TODO2 We do not remember why we needed to delay eval of menu item text. Presumably because otherwise we evaluate the JavaScript too soon. *)
                    let item_2 = lazy { item_1 with text = eval_js_string_with_menu_variables item_1.text menu_variables }
                    match item_1.conditional with
                        | Some conditional ->
                            if eval_js_boolean_with_menu_variables conditional menu_variables then Some item_2.Value
                            else None
                        | None -> Some item_2.Value
                )
        }
        runner_components.current.menu.current.show menu_data_2 true (Some command_queue_item_id)

    {
        command = Some command
        debug_data = menu_data_1.ToString ()
        behavior = menu_behavior
        components_used = Set.singleton Menu
        next_command_scene_id = current_scene_id
        next_command_id = next_command_id
    }

let private handle_image_map
    (runner_components : IRefValue<Runner_Components>)
    (current_scene_id : int<scene_id>)
    (next_command_id : int<command_id> option)
    (image_map_data_1 : Image_Map_Data)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    let command = fun (command_queue_item_id : int<runner_queue_item_id>) ->

        let image_map_data_2 = {
            image_map_data_1 with
                items = image_map_data_1.items |> List.filter (fun item ->
                    match item.conditional with
                    | Some conditional -> eval_js_boolean_with_menu_variables conditional menu_variables
                    | None -> true
                )
        }
        runner_components.current.image_map.current.fade_in image_map_data_2 image_map_data_2.transition_time command_queue_item_id

    {
        command = Some command
        debug_data = image_map_data_1.ToString ()
        behavior = image_map_behavior
        components_used = Set.singleton Image_Map
        next_command_scene_id = current_scene_id
        next_command_id = next_command_id
    }

(* Main functions *)

let get_command_data
    (scene_id : int<scene_id>)
    (runner_components : IRefValue<Runner_Components>)
    (command : Command_Post_Parse)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    match command.command with

    | Command_Post_Parse_Type.Command command_1 -> handle_command runner_components scene_id   command.next_command_id command_1 menu_variables

    | Command_Post_Parse_Type.If command_2 -> handle_if scene_id command.next_command_id command_2 menu_variables

    | Command_Post_Parse_Type.End_If ->
        {
            command = None
            debug_data = "End_If"
            behavior = end_if_behavior
            components_used = Set.empty
            next_command_scene_id = scene_id
            next_command_id = command.next_command_id
        }

    | Command_Post_Parse_Type.Menu command_3 -> handle_menu runner_components scene_id command.next_command_id command_3 menu_variables

    | Command_Post_Parse_Type.Image_Map command_4 -> handle_image_map runner_components scene_id command.next_command_id command_4 menu_variables

    | Command_Post_Parse_Type.End_Image_Map transition_time ->
        {
            command = Some <| fun (command_queue_item_id : int<runner_queue_item_id>) -> runner_components.current.image_map.current.fade_out transition_time command_queue_item_id
            debug_data = "End_Image_Map"
            behavior = end_image_map_behavior
            components_used = Set.singleton Runner_Component_Names.Image_Map
            next_command_scene_id = scene_id
            next_command_id = command.next_command_id
        }
