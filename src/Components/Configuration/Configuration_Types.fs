module Configuration_Types

open Background
open Character_Types
open Dialogue_Box_Types
open Key_Bindings
open Notification_Types
open Units_Of_Measure

(* Types *)

// TODO2 #configuration Runner_History_Configuration and Runner_Configuration should be in Runner_Types, but it hasn't been declared yet.
type Runner_History_Configuration = {
    max_history_length : int
}

type Mouse_Configuration = {
    wheel_action_elapsed_time_threshold : int<milliseconds>
}

type Runner_Configuration = {
(* These fields cannot be IRefValue because we need to serialize this type. *)
    background_configuration : Background_Configuration
    characters_configuration : Characters_Configuration
    dialogue_box_configuration : Dialogue_Box_Configuration
    temporary_notifications_configuration : Notifications_Configuration
    history_configuration : Runner_History_Configuration
    key_bindings_configuration : Key_Bindings_Configuration
    mouse_configuration : Mouse_Configuration
}

type Configuration_State = {
    is_visible : bool
}

type Configuration_Message =
    | Show
    | Hide

(* Interfaces *)

type I_Configuration =
    abstract member show : unit -> unit
    abstract member hide : unit -> unit
    abstract member is_visible : unit -> bool

(* Consts *)

let local_storage_name = "vnf_configuration"

let private default_background_configuration : Background_Configuration = {
    placeholder = ()
}
let private default_characters_configuration : Characters_Configuration = {
    placeholder = ()
}
let private default_dialogue_box_configuration : Dialogue_Box_Configuration = {
    typing_speed = 0<milliseconds>
}
let private default_temporary_notifications_configuration : Notifications_Configuration = {
    display_time = 5.0<seconds>
    transition_time = 1.0<seconds>
}

let private default_history_configuration : Runner_History_Configuration = {
(* Note 0 = unlimited. *)
    max_history_length = 20
}

let min_mouse_wheel_action_elapsed_time_threshold = 0<milliseconds>
let max_mouse_wheel_action_elapsed_time_threshold = 1000<milliseconds>
let default_mouse_wheel_action_elapsed_time_threshold = 100<milliseconds>

let private default_mouse_configuration : Mouse_Configuration = {
    wheel_action_elapsed_time_threshold = default_mouse_wheel_action_elapsed_time_threshold
}

let default_configuration = {
    background_configuration = default_background_configuration
    characters_configuration = default_characters_configuration
    dialogue_box_configuration = default_dialogue_box_configuration
    temporary_notifications_configuration = default_temporary_notifications_configuration
    history_configuration = default_history_configuration
    key_bindings_configuration = get_default_key_bindings_configuration ()
    mouse_configuration = default_mouse_configuration
}

(* Note 0 = unlimited. *)
let min_max_history_length = 0
let max_max_history_length = 99
