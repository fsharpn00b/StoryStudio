module Runner_Queue_Helpers_1

open Command_Types
open Log
open Parser_1_Helpers
open Runner_Types
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Queue_Helpers_1"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

let if_behavior = Continue_Immediately { run_queue_now = false; autosave = false }
let end_if_behavior = Continue_Immediately { run_queue_now = false; autosave = false }
let menu_behavior = Wait_For_Callback { continue_afterward = false; add_to_history = true; autosave = true }
let image_map_behavior = Wait_For_Callback { continue_afterward = false; add_to_history = true; autosave = true }
let end_image_map_behavior = Wait_For_Callback { continue_afterward = true; add_to_history = false; autosave = false }

let private initial_command_queue_item_id = 0<command_queue_item_id>

(* Helper functions *)

let command_to_behavior (command : Command_Type) : Command_Behavior =
    match command with
(* Pause and wait for the transition to complete. *)
    | Background_Fade_In _
    | Background_Fade_Out _
    | Background_Cross_Fade _
    | Character_Fade_In _
    | Character_Fade_Out _
    | Character_Cross_Fade _
    | Character_Move _
    | Fade_Out_All _
    | Dialogue_Box_Show
    | Dialogue_Box_Hide -> Wait_For_Callback { continue_afterward = true; add_to_history = false; autosave = false }
(* Pause and wait for the user to continue.

We set continue_after_running to false because, even if the dialogue box has its typing speed is 0 (meaning text is shown at once and in full), we let it signal that the transition is complete, to simplify the logic here.
*)
    | Dialogue _ -> Wait_For_Callback { continue_afterward = false; add_to_history = true; autosave = false }
(* Do not pause or autosave. *)
    | Music_Play _
    | Music_Stop
    | Temporary_Notification _
    | Permanent_Notification _
    | Hide_Permanent_Notification
// TODO2 Possibly these should not be commands, or should at least be a separate subtype.
    | JavaScript_Inline _
    | JavaScript_Block _ -> Continue_Immediately { run_queue_now = true; autosave = false }
    | Eval _ -> Continue_Immediately { run_queue_now = false; autosave = false }
(* Do not pause, but do autosave. *)

    | Jump _ -> Continue_Immediately { run_queue_now = false; autosave = true }

let command_to_component_ids (command : Command_Type) : Runner_Component_Names Set =
    match command with
    | Background_Fade_In _
    | Background_Fade_Out _
    | Background_Cross_Fade _ -> Set.singleton Background
    | Character_Fade_In _
    | Character_Fade_Out _
    | Character_Cross_Fade _
    | Character_Move _ -> Set.singleton Characters
    | Fade_Out_All _ -> Set.ofList [Background; Characters; Dialogue_Box]
    | Dialogue_Box_Show
    | Dialogue_Box_Hide
    | Dialogue _ -> Set.singleton Dialogue_Box
(* The Music component does not have transitions. *)
    | Music_Play _
    | Music_Stop
(* The Notifications component does not notify Runner when it completes a transition. *)
    | Temporary_Notification _
    | Permanent_Notification _
    | Hide_Permanent_Notification
    | JavaScript_Inline _
    | JavaScript_Block _
    | Jump _
    | Eval _ -> Set.empty

let get_script_name_and_line_number
    (scenes : Scene_Map)
    (calling_function : string)
    (error_data : Command_Error_Data_2)
    (known_error_data : (string * obj) list)
    : string * int =

    let scene =
        match scenes.TryFind error_data.scene_id with
        | Some scene -> scene
        | None ->
            error
                $"{calling_function} > get_script_name_and_line_number"
                "While trying to get the scene name and line number for a command that raised an error, we encountered an additional error: the scene ID for this command is unknown."
                (known_error_data @ [
                    "scene_id", error_data.scene_id
                    "known_scenes", scenes |> Seq.map (fun kv ->
                        $"scene_id: {kv.Key}. scene_name: {kv.Value.name}"
                    ) :> obj
                ]) |> invalidOp
    scene.name, get_script_line_number scene.content error_data.script_text_index

let get_initial_queue () : Runner_Queue =
    
    Queue_Idle {
        next_command = {
            next_command_queue_item_id = initial_command_queue_item_id
            next_command = Some {
                next_command_scene_id = entry_scene_id
                next_command_id = scene_initial_command_id
            }
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
