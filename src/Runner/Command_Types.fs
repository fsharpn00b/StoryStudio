module Command_Types

open System.Text.RegularExpressions

open Character_Types
open Background
open Image_Map
open Menu
open Temporary_Notification
open Units_Of_Measure

(* The CN_ prefix is to prevent collisions with type Command. *)
type Command_Name =
    | CN_Music_Play
    | CN_Music_Stop
    | CN_Background_Fade_In
    | CN_Background_Fade_Out
    | CN_Background_Cross_Fade
    | CN_Character_Fade_In
    | CN_Character_Fade_Out
    | CN_Character_Cross_Fade
    | CN_Fade_Out_All
    | CN_Dialogue_Box_Show
    | CN_Dialogue_Box_Hide
    | CN_Temporary_Notification
    | CN_Permanent_Notification
    | CN_JavaScript_Inline
    | CN_If
    | CN_Else_If
    | CN_Else
    | CN_End_If
    | CN_Jump
    | CN_Menu
    | CN_End_Menu
    | CN_Image_Map
    | CN_End_Image_Map

type Command_Parameter_Type =
    | Any_Min_Length_0
    | Any_Min_Length_1
    | Word
    | Int
    | Float
(* We do not use this for now. *)
//    | Other of {| pattern : string |}

type Command_Parameter = {
    name : string
    description : string
    type' : Command_Parameter_Type
}

type Command_Pattern_1 = {
    name : Command_Name
    pattern : string
    parameters : Command_Parameter list
}

type Command_Pattern_2 = {
    name : Command_Name
    pattern : string
    parameters : Map<string, Command_Parameter_Type>
    parameters_regex : Regex option
}

type Command_Parameters = {
    ints : Map<string, int>
    floats : Map<string, float>
    strings : Map<string, string>
}

(* This is passed to the debug, warn, or error function. *)
type Parser_Error = string * ((string * obj) list)

type Dialogue_Data = {
    character_short_name : string
    character_full_name : string
    text : string
    javascript_interpolations : string list
}

type Command =
    | Music_Play of string
    | Music_Stop
    | Background_Fade_In of Background_Fade_In_Data
    | Background_Fade_Out of Background_Fade_Out_Data
    | Background_Cross_Fade of Background_Cross_Fade_Data
    | Character_Fade_In of Character_Fade_In_Data
    | Character_Fade_Out of Character_Fade_Out_Data
    | Character_Cross_Fade of Character_Cross_Fade_Data
    | Fade_Out_All of Fade_Transition_Time
    | Dialogue_Box_Show
    | Dialogue_Box_Hide
    | Dialogue of Dialogue_Data
    | Temporary_Notification of Notification_Data_1
    | Permanent_Notification of Notification_Data_1
    | JavaScript_Inline of string
    | JavaScript_Block of string
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
type Command_Pre_Parse =
    | Command of Command
    | If of string
    | Else_If of string
    | Else
    | End_If
    | Menu of Menu_Data_1
    | Image_Map of Image_Map_Data
    | End_Image_Map of Fade_Transition_Time

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
    | Command of Command
    | If of If_Block
    | End_If
    | Menu of Menu_Data_1
    | Image_Map of Image_Map_Data
    | End_Image_Map of Fade_Transition_Time

type Command_Post_Parse =
    {
        id : int<command_id>
        next_command_id : int<command_id> option
        parent_command_id : int<command_id> option
        command : Command_Post_Parse_Type
    }

type Scene = Map<int<command_id>, Command_Post_Parse>
type Scene_Map = Map<int<scene_id>, Scene>

type Script = {
    id : int<scene_id>
    name : string
    content : string
}
