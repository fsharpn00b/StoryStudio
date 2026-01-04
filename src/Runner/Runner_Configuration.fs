module Runner_Configuration

// String.Empty
open System

// IRefValue
open Feliz

open Background
open Character_Types
open Configuration
open Dialogue_Box_Types
open Key_Bindings
open Log
open Runner_Types
open Temporary_Notification
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Runner_Configuration"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

let private default_background_configuration : Background_Configuration = {
    placeholder = ()
}
let private default_characters_configuration : Characters_Configuration = {
    placeholder = ()
}
let private default_dialogue_box_configuration : Dialogue_Box_Configuration = {
    typing_speed = 0<milliseconds>
}
let private default_temporary_notifications_configuration : Temporary_Notifications_Configuration = {
    display_time = 5.0<seconds>
    transition_time = 1.0<seconds>
}

let default_configuration = {
    background_configuration = default_background_configuration
    characters_configuration = default_characters_configuration
    dialogue_box_configuration = default_dialogue_box_configuration
    temporary_notifications_configuration = default_temporary_notifications_configuration
    key_bindings_configuration = get_default_key_bindings_configuration ()
}

(* Main functions *)

let set_configuration
    (runner_components : IRefValue<Runner_Components>)
    (old_configuration : IRefValue<Runner_Configuration>)
    (new_configuration : Runner_Configuration)
    : unit =
    do
        runner_components.current.background.current.set_configuration new_configuration.background_configuration
        runner_components.current.characters.current.set_configuration new_configuration.characters_configuration
        runner_components.current.dialogue_box.current.set_configuration new_configuration.dialogue_box_configuration
        runner_components.current.notifications.current.set_configuration new_configuration.temporary_notifications_configuration
        old_configuration.current <- new_configuration
