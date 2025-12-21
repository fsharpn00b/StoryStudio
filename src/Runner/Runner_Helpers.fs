module Runner_Helpers

// Environment.NewLine
open System

// console
open Browser.Dom
// IRefValue
open Feliz

open Command_Types
open Character_Types
open Menu
open JavaScript_Interop
open Log
open Runner_Types
open Scripts
open Units_Of_Measure
open Utilities

(* Debug *)

let debug_module_name = "Runner_Helpers"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Functions *)

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
// TODO2 Possibly these should not be commands, or should at least be a separate subtype.
    | JavaScript_Inline _
    | JavaScript_Block _ -> Continue_Immediately { autosave = false }
    | Jump _ -> Continue_Immediately { autosave = true }
// TODO1 Just give If, EndIf, and Menu Command_Behaviors. They are not Commands. Add a subtype to them to contain behavior.
(* See also If in handle_if (), EndIf in get_command_data (), and Menu in handle_menu (). They do not have Command_Behaviors, but their behaviors are defined in those functions. *)

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
    | Music_Play _
    | Music_Stop
    | JavaScript_Inline _
    | JavaScript_Block _
    | Jump _ -> Set.empty

let component_id_to_component
    (runner_components : IRefValue<Runner_Components>)
    (component_id : Runner_Component_Names)
    : I_Transitionable =

    match component_id with
    | Background -> runner_components.current.background.current :?> I_Transitionable
    | Characters -> runner_components.current.characters.current :?> I_Transitionable
    | Dialogue_Box -> runner_components.current.dialogue_box.current :?> I_Transitionable
    | Menu -> runner_components.current.menu.current :?> I_Transitionable

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

        | Background_Fade_In command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) -> do runner_components.current.background.current.fade_in command_2.new_url command_2.transition_time command_queue_item_id

        | Background_Fade_Out command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) -> do runner_components.current.background.current.fade_out command_2.transition_time command_queue_item_id

        | Background_Cross_Fade command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) -> do runner_components.current.background.current.cross_fade command_2.new_url command_2.transition_time command_queue_item_id

        | Character_Fade_In command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) -> do runner_components.current.characters.current.fade_in command_2.character_short_name command_2.url command_2.position command_2.transition_time command_queue_item_id

        | Character_Fade_Out command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) -> runner_components.current.characters.current.fade_out command_2.character_short_name command_2.transition_time command_queue_item_id

        | Character_Cross_Fade command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) -> runner_components.current.characters.current.cross_fade command_2.character_short_name command_2.url command_2.transition_time command_queue_item_id

        | Fade_Out_All transition_time ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) ->
                runner_components.current.dialogue_box.current.hide true <| Some command_queue_item_id
                runner_components.current.characters.current.fade_out_all transition_time command_queue_item_id
                runner_components.current.background.current.fade_out transition_time command_queue_item_id
                
        | Dialogue_Box_Show ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) -> runner_components.current.dialogue_box.current.show true <| Some command_queue_item_id

        | Dialogue_Box_Hide ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) -> runner_components.current.dialogue_box.current.hide true <| Some command_queue_item_id

        | Dialogue command_2 ->
            Some <| fun (command_queue_item_id : int<command_queue_item_id>) -> runner_components.current.dialogue_box.current.type_dialogue command_2.character_full_name (eval_js_string_with_menu_variables command_2.text menu_variables) command_queue_item_id

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
        behavior = Continue_Immediately { autosave = false }
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
    let command = fun (command_queue_item_id : int<command_queue_item_id>) ->

        let menu_data_2 = {
            menu_data_1 with
                description = eval_js_string_with_menu_variables menu_data_1.description menu_variables
                items = menu_data_1.items |> List.choose (fun item_1 ->
(* TODO2 We do not remember why we needed to delay eval of menu item text. *)
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
        behavior = Wait_For_Callback { continue_afterward = false; add_to_history = true; autosave = true }
        components_used = Set.singleton Menu
        next_command_scene_id = current_scene_id
        next_command_id = next_command_id
    }

let get_command_data
    (scene_id : int<scene_id>)
    (runner_components : IRefValue<Runner_Components>)
    (command : Command_Post_Parse)
    (menu_variables : Menu_Variables)
    : Runner_Command_Data =

    match command.command with

    | Command_Post_Parse_Type.Command command_3 -> handle_command runner_components scene_id   command.next_command_id command_3 menu_variables

(* TODO1 We could have a function Command_Post_Parse_Type -> Runner_Command_Data. Its job is to translate Command_Behavior to Runner_Command_Data, and in the case of If/EndIf/Menu, which are not Commands but still have behaviors, to centralize the definitions of those behaviors.
*)
    | Command_Post_Parse_Type.If command_4 -> handle_if scene_id command.next_command_id command_4 menu_variables

    | Command_Post_Parse_Type.End_If ->
        {
            command = None
            debug_data = "End_If"
            behavior = Continue_Immediately { autosave = false }
            components_used = Set.empty
            next_command_scene_id = scene_id
            next_command_id = command.next_command_id
        }

    | Command_Post_Parse_Type.Menu command_5 -> handle_menu runner_components scene_id command.next_command_id command_5 menu_variables
