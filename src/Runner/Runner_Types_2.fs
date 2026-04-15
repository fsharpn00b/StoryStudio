module Runner_Types_2

// console, window
open Browser.Dom
// IRefValue
open Feliz

open Background
open Character_Types
open Command_Menu
open Command_Types
open Configuration
open Dialogue_Box_Types
open Image_Map
open JavaScript_Interop_1
open Key_Bindings
open Menu
open Music
open Notification_Types
open Runner_Types_1
open Save_Load_Types
open Units_Of_Measure

(* Types - public *)

type Plugin_Data = {
    path : string
(* TODO1 #plugins We must store the interface as an obj because each component exposes a different interface and we do not know how to statically determine the interface type. If we did, we are not sure how we would store different interface types in a single map. Presumably they would all need to inherit from a base interface type, but there are no base interface methods we require, so they might as well inherit from obj.
In any case, we do not use these interfaces inside the framework. Instead, they are used by JavaScript written by the author. JavaScript does not need to cast the obj to the interface type. It is enough to say, for example:
window.Interfaces.<interface>.current.<method>
(end)

- However, we should let a plugin author provide a TypeScript definition of the interface that we can then use to check the JavaScript code.
*)
    interface_ref : IRefValue<obj>
}

type Plugins_Data = Map<string, Plugin_Data>

type Runner_Component_Interfaces = {
    background : IRefValue<I_Background>
    characters : IRefValue<I_Characters>
    dialogue_box : IRefValue<I_Dialogue_Box>
    menu : IRefValue<I_Menu>
    image_map : IRefValue<I_Image_Map>
    save_load : IRefValue<I_Save_Load>
    music : IRefValue<I_Music>
    configuration : IRefValue<I_Configuration>
    command_menu : IRefValue<I_Command_Menu>
    notifications : IRefValue<I_Notifications>
    plugins : Plugins_Data
}

type Runner_History = {
    configuration : Runner_History_Configuration
    current_index : int option
    history : Runner_Saveable_State list
    notify_history_changed : unit -> unit
}

type Runner_Component_Names =
    | Background
    | Characters
    | Dialogue_Box
    | Menu
    | Image_Map

type Runner_Command_Signature = int<command_queue_item_id> -> unit

type Runner_Command_Data = {
    command : Runner_Command_Signature option
    error_data : Command_Error_Data_2
    behavior : Command_Behavior
    components_used : Runner_Component_Names Set
    next_command : Next_Command_Data option
}

type Runner_Queue_Item = {
    command_data : Runner_Command_Data
    order_in_queue : int<command_queue_order>
    components_used_by_command : Runner_Component_Names Set
}

type Runner_Queue_State_Idle_Data = {
    next_command : Runner_Queue_Next_Command_Data
(* See notes in Command_Queue_State_Running_Data. *)
    add_to_history : bool
    autosave : bool
(* We do not need to carry the JavaScript state in the queue because that is stored in the browser's window object. We can get it with back_up_state ().
We could also store the menu variables that way, but we use them more often (whenever we evaluate a JavaScript statement or condition) and we feel it makes the code clearer to store them in the queue.
See also notes in Runner_Transition.get_notify_menu_selection ().
*)
    menu_variables : Menu_Variables
}

type Runner_Queue_Command_Map = Map<int<command_queue_item_id>, Runner_Queue_Item>

type Runner_Queue_State_Loading_Data = {
    commands : Runner_Queue_Command_Map
    next_command_data : Runner_Queue_Next_Command_Data
    components_used_by_commands : Runner_Component_Names Set
(* See notes in Command_Queue_State_Running_Data. *)
    autosave : bool
    menu_variables : Menu_Variables
}

type Runner_Queue_State_Running_Data = {
    commands : Runner_Queue_Command_Map
    next_command_data : Runner_Queue_Next_Command_Data
    components_used_by_commands : Runner_Component_Names Set
    continue_after_finished : bool
(* This determines whether, after we run the last command in the queue, we add the current state to the history. We initially set it to the inverse of continue_after_finished. We do not simply use continue_after_finished because we might need to set that to false to halt running commands if the player opens the save/load game screen or rolls back/forward. *)
    add_to_history : bool
(* This determines whether, after we run the last command in the queue, we autosave. *)
    autosave : bool
    menu_variables : Menu_Variables
}

// TODO2 This item originally applied to Command_State. We might need another state, Paused (meaning we are waiting for user input). See also the items in Runner_Transition.notify_transition_complete () and Runner_Run.get_next_command (). For now we do not think this would be useful. Command_State is used to carry information between the various Runner_Run functions, but it does not really track state or determine behavior.
type Runner_Queue =
    | Queue_Idle of Runner_Queue_State_Idle_Data
    | Queue_Loading of Runner_Queue_State_Loading_Data
    | Queue_Running of Runner_Queue_State_Running_Data
    | Queue_Interrupting of Runner_Queue_State_Running_Data
    | Queue_Done

type Run_Reason =
    | Initial_Run
    | Player_Run
    | Handle_Queue_Empty
    | Notify_Menu_Selection
    | Notify_Image_Map_Selection

type Runner_State = {
    history : IRefValue<Runner_History>
    queue : IRefValue<Runner_Queue>
    scenes : IRefValue<Scene_Map>
    runner_component_interfaces : IRefValue<Runner_Component_Interfaces>
    parser : Parser
}

(* Interfaces *)

type I_Runner =
    abstract member run : Run_Reason -> unit
    abstract member get_key_bindings : unit -> Key_To_Key_Binding_Name
    abstract member show_or_hide_configuration_screen : unit -> unit
// We do not use this for now.
//    abstract member hide_configuration_screen : unit -> unit
    abstract member handle_escape_key : unit -> unit
    abstract member show_saved_game_screen : Saved_Game_Action -> unit
// We do not use this for now.
//    abstract member hide_saved_game_screen : unit -> unit
    abstract member show_or_hide_ui : unit -> unit
    abstract member download_screenshot : unit -> unit
    abstract member quicksave : unit -> unit
    abstract member export_current_game_to_file : unit -> unit
    abstract member import_current_game_from_file : unit -> unit
    abstract member undo : unit -> unit
    abstract member redo : unit -> unit
(* These are for debugging. *)
    abstract member show_queue : unit -> unit
    abstract member show_characters : unit -> unit
    abstract member show_background : unit -> unit
    abstract member show_menu_variables : unit -> unit
