module Configuration_Helpers

// DateTime
open System

// navigator, Types (also provides console, window)
open Browser
// a, Element, HTMLCanvasElement, HTMLElement, HTMLTextAreaElement
open Browser.Types
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz
// Decode, Encode
open Thoth.Json

open Configuration_Types
open Dialogue_Box_Types
open Key_Bindings
open Log
open Notification_Types
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Configuration"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

let private characters_per_second_to_delay_between_characters
    (characters_per_second : int)
    : int<milliseconds>
    =
    int (1000 / characters_per_second) |> LanguagePrimitives.Int32WithMeasure

let delay_between_characters_to_characters_per_second
    (delay_between_characters : int)
    : int =
    int (1000 / delay_between_characters)

let get_configuration_from_local_storage () : Runner_Configuration option =
    match localStorage.getItem local_storage_name with
    | null -> None
    | json ->
        match Decode.Auto.fromString<Runner_Configuration> json with
        | Ok configuration -> Some configuration
        | Error message -> error "get_configuration_from_local_storage" "Failed to deserialize configuration." ["json", json; "error_message", message] |> invalidOp

let private set_configuration_in_local_storage
    (configuration : IRefValue<Runner_Configuration>) : unit =
    let json = Encode.Auto.toString (0, configuration.current)
    do localStorage.setItem (local_storage_name, json)

// TODO1 #configuration Change this to a validation function. If any of these fail, show alert and bail.
let private update_configuration
    (configuration : IRefValue<Runner_Configuration>)
    (set_configuration : Runner_Configuration -> unit)
    (new_typing_speed_value_1 : HTMLTextAreaElement)
    (new_mouse_wheel_action_elapsed_time_threshold_value_1 : HTMLTextAreaElement)
    (new_notification_display_time_value_1 : HTMLTextAreaElement)
    (new_notification_transition_time_value_1 : HTMLTextAreaElement)
    (new_max_history_length_value_1 : HTMLTextAreaElement)
    (new_key_bindings_configuration : Key_Bindings_Configuration)
    : unit =

    match Int32.TryParse new_typing_speed_value_1.value with
    | true, new_value_2 ->
        let new_value_3 =
            if new_value_2 > 0 then characters_per_second_to_delay_between_characters new_value_2
            else 0<milliseconds>
        do
            configuration.current <- { configuration.current with dialogue_box_configuration = { typing_speed = new_value_3 }}
    | _ -> ()

    match Int32.TryParse new_notification_display_time_value_1.value with
    | true, new_value_2 ->
        let new_value_3 =
            if new_value_2 < int min_temporary_notification_display_time then int min_temporary_notification_display_time
            elif new_value_2 > int max_temporary_notification_display_time then int max_temporary_notification_display_time
            else new_value_2

        do configuration.current <- { configuration.current with temporary_notifications_configuration = { configuration.current.temporary_notifications_configuration with display_time = new_value_3 |> float |> LanguagePrimitives.FloatWithMeasure } }
    | _ -> ()

    match Int32.TryParse new_notification_transition_time_value_1.value with
    | true, new_value_2 ->
        let new_value_3 =
            if new_value_2 < int min_notification_transition_time then int min_notification_transition_time
            elif new_value_2 > int max_notification_transition_time then int max_notification_transition_time
            else new_value_2

        do configuration.current <- { configuration.current with temporary_notifications_configuration = { configuration.current.temporary_notifications_configuration with transition_time = new_value_3 |> float |> LanguagePrimitives.FloatWithMeasure } }
    | _ -> ()

    match Int32.TryParse new_max_history_length_value_1.value with
    | true, new_value_2 ->
        let new_value_3 =
            if new_value_2 < min_max_history_length then min_max_history_length
            elif new_value_2 > max_max_history_length then max_max_history_length
            else new_value_2
        do configuration.current <- { configuration.current with history_configuration = { max_history_length = new_value_3 } }
    | _ -> ()

    match Int32.TryParse new_mouse_wheel_action_elapsed_time_threshold_value_1.value with
    | true, new_value_2 ->
        let new_value_3 = new_value_2 |> LanguagePrimitives.Int32WithMeasure
        let new_value_4 =
            if new_value_3 < min_mouse_wheel_action_elapsed_time_threshold then min_mouse_wheel_action_elapsed_time_threshold
            elif new_value_3 > max_mouse_wheel_action_elapsed_time_threshold then max_mouse_wheel_action_elapsed_time_threshold
            else new_value_3
        do configuration.current <- { configuration.current with mouse_configuration = { configuration.current.mouse_configuration with wheel_action_elapsed_time_threshold = new_value_4 } }
    | _ -> ()

// TODO1 #configuration Why are we doing this here and not in set_configuration ()?
    do configuration.current <- { configuration.current with key_bindings_configuration = new_key_bindings_configuration }

    set_configuration configuration.current
    set_configuration_in_local_storage configuration

let handle_save_button_click
    (configuration : IRefValue<Runner_Configuration>)
    (set_configuration : Runner_Configuration -> unit)
    (dispatch : Configuration_Message -> unit)
    : unit =

// TODO1 #configuration This is another layer of validation that should be in a validation function.
    match get_key_bindings_configuration () with
    | None -> ()
    | Some key_bindings_configuration ->
        update_configuration configuration set_configuration (document.getElementById "txt_typing_speed" :?> HTMLTextAreaElement) (document.getElementById "txt_mouse_wheel_action_elapsed_time_threshold" :?> HTMLTextAreaElement) (document.getElementById "txt_notification_display_time" :?> HTMLTextAreaElement) (document.getElementById "txt_notification_transition_time" :?> HTMLTextAreaElement) (document.getElementById "txt_max_history_length" :?> HTMLTextAreaElement) key_bindings_configuration
        dispatch Hide
