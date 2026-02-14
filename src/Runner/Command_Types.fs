module Command_Types

open Character_Types
open Background
open Image_Map
open Menu
open Notification_Types
open Units_Of_Measure

type Dialogue_Data = {
    character_short_name : string
    character_full_name : string
    text : string
    javascript_interpolations : string list
}

type JavaScript_Data = {
    code : string
    script_text_index : int
}

type Command_Type =
    | Music_Play of string
    | Music_Stop
    | Background_Fade_In of Background_Fade_In_Data
    | Background_Fade_Out of Background_Fade_Out_Data
    | Background_Cross_Fade of Background_Cross_Fade_Data
    | Character_Fade_In of Character_Fade_In_Data
    | Character_Fade_Out of Character_Fade_Out_Data
    | Character_Cross_Fade of Character_Cross_Fade_Data
    | Fade_Out_All of Transition_Time
    | Character_Move of Character_Move_Data_2
    | Dialogue_Box_Show
    | Dialogue_Box_Hide
    | Dialogue of Dialogue_Data
    | Temporary_Notification of Notification_Data_1
    | Permanent_Notification of Notification_Data_1
    | Hide_Permanent_Notification
    | JavaScript_Inline of JavaScript_Data
    | JavaScript_Block of JavaScript_Data
    | Jump of int<scene_id>

type Wait_For_Callback_Behavior = {
    continue_afterward : bool
    add_to_history : bool
    autosave : bool
}

type Continue_Immediately_Behavior = {
    autosave : bool
}

(* Continue_Immediately or continue_afterward mean we can automatically progress to the next command after the current one runs. Wait_For_Callback means we need to wait until the current command finishes.

For example:
                    
                    Continue_Immediately    Wait_For_Callback
                                            continue_afterward
Background_Fade_In  false                   true
Dialogue            false                   false
JavaScript_Inline   true

Continue_Immediately implies the command finishes immediately.
*)
type Command_Behavior =
    | Wait_For_Callback of Wait_For_Callback_Behavior
// TODO2 We should also have an auto-play setting (say, continue after x seconds instead of immediately after finishing.) A setting of 0 could mean continue manually.
    | Continue_Immediately of Continue_Immediately_Behavior

(* This is after we have matched a command but before we have parsed it. Parsing mostly means to assign command IDs and deal with If/Else_If/Else/End_If statements. *)
type Command_Pre_Parse_Type =
    | Command of Command_Type
    | If of string
    | Else_If of string
    | Else
    | End_If
    | Menu of Menu_Data_1
    | Image_Map of Image_Map_Data
    | End_Image_Map of Transition_Time

type Command_Error_Data_1 = {
    source : string
    script_text_index : int
}

type Command_Pre_Parse_1 = {
    error_data : Command_Error_Data_1
    command : Command_Pre_Parse_Type
}

type Command_Error_Data_2 = {
    source : string
    scene_id : int<scene_id>
    script_text_index : int
}

type Command_Pre_Parse_2 = {
    error_data : Command_Error_Data_2
    command : Command_Pre_Parse_Type
}

(* We cannot simply use next_command_id for an Else_If block's commands because it is not a full Command_Post_Parse. It is just part of If_Block. *)
type Else_If_Block_With_Id = {
    conditional : string
    child_command_id : int<command_id>
}

type If_Block = {
    conditional : string
(* We could just use next_command_id for the If block's commands, but child_command_id is consistent with Else_If_Block_With_Id. *)
    child_command_id : int<command_id>
    else_if_blocks : Else_If_Block_With_Id list
    else_block : int<command_id> option
}

type Command_Post_Parse_Type =
    | Command of Command_Type
    | If of If_Block
    | End_If
    | Menu of Menu_Data_1
    | Image_Map of Image_Map_Data
    | End_Image_Map of Transition_Time

type Command_Post_Parse =
    {
        id : int<command_id>
        next_command_id : int<command_id> option
        parent_command_id : int<command_id> option
        command : Command_Post_Parse_Type
        error_data : Command_Error_Data_2
    }

type Scene_Data = {
    name : string
    content : string
    commands : Map<int<command_id>, Command_Post_Parse>
}
type Scene_Map = Map<int<scene_id>, Scene_Data>

type Script = {
    id : int<scene_id>
    name : string
    content : string
}
