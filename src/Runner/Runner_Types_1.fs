module Runner_Types_1

open Background
open Dialogue_Box_Types
open Character_Types
open Menu
open Image_Map
open JavaScript_Interop_1
open Music
open Notification_Types
open Units_Of_Measure

(* These must be declared before Save_Load_Validation. *)

type Runner_Saveable_State_Component_Data = {
    background : Background_State
    dialogue : Dialogue_Box_Saveable_State
    characters : Characters_Saveable_State
    menu : Menu_Saveable_State
    image_map : Image_Map_State
    music : Music_State
    notifications : Notifications_Saveable_State
    javascript_state : string
}

(* A field that uses this type should always be optional. None means there is no next command. *)
type Next_Command_Data = {
    next_command_scene_id : int<scene_id>
    next_command_id : int<command_id>
}

type Runner_Queue_Next_Command_Data = {
    next_command_queue_item_id : int<command_queue_item_id>
    next_command : Next_Command_Data option
}

type Runner_Saveable_State_Running_Data = {
    next_command : Runner_Queue_Next_Command_Data
(* We clear the history when we load a saved game. This determines whether, when we load a saved game, we re-add the current state to the history. *)
    add_to_history : bool
(* This determines whether, when we load a saved game, we then autosave the current state. *)
    autosave : bool
    component_data : Runner_Saveable_State_Component_Data
    menu_variables : Menu_Variables
}

type Runner_Saveable_State =
    | Runner_Saveable_State_Running of Runner_Saveable_State_Running_Data
(* TODO2 We considered including add_to_history in Runner_Saveable_State_Done, but when we load a saved game, we clear the history, and if the saved game has runner state Queue_Done, what is the point of adding a single state to the history? The player still cannot undo/redo.
Does Runner_Saveable_State_Running_Data.add to history play any role in undo/redo? It should not. Undo/redo just gets an existing saved state. It does not need to know whether that state should be added to the history, because it already was.
*)
    | Runner_Saveable_State_Done of Runner_Saveable_State_Component_Data
