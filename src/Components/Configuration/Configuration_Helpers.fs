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
open Utilities

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

let private validate_configuration
    (configuration : IRefValue<Runner_Configuration>)
    (new_dialogue_typing_speed_value_1 : HTMLTextAreaElement)
    (new_mouse_wheel_action_elapsed_time_threshold_value_1 : HTMLTextAreaElement)
    (new_notification_display_time_value_1 : HTMLTextAreaElement)
    (new_notification_transition_time_value_1 : HTMLTextAreaElement)
    (new_max_history_length_value_1 : HTMLTextAreaElement)
    : Result<unit, string list> =

    let key_bindings_result_1 = get_key_bindings_configuration ()

    let dialogue_typing_speed_result_1 =
        match Int32.TryParse new_dialogue_typing_speed_value_1.value with
        | true, new_value when new_value > 0 ->
            new_value |> characters_per_second_to_delay_between_characters |> Ok
        | true, new_value when 0 = new_value -> Ok 0<milliseconds>                
        | _ -> Error "Dialogue typing speed is not a number or is too low (minimum 0)."

    let notification_display_time_result_1 =
        match Double.TryParse new_notification_display_time_value_1.value with
        | true, new_value_1 ->
            let new_value_2 = new_value_1 |> LanguagePrimitives.FloatWithMeasure
            if new_value_2 < min_temporary_notification_display_time then
                Error $"Notification display time is too low (minimum {min_temporary_notification_display_time})."
            elif new_value_2 > max_temporary_notification_display_time then
                Error $"Notification display time is too high (maximum {max_temporary_notification_display_time})."
            else new_value_2 |> Ok
        | _ -> Error "Notification display time is not a number."

    let notification_transition_time_result_1 =
        match Double.TryParse new_notification_transition_time_value_1.value with
        | true, new_value_1 ->
            let new_value_2 = new_value_1 |> LanguagePrimitives.FloatWithMeasure
            if new_value_2 < min_notification_transition_time then
                Error $"Notification transition time is too low (minimum {min_notification_transition_time})."
            elif new_value_2 > max_notification_transition_time then
                Error $"Notification transition time is too high (maximum {max_notification_transition_time})."
            else new_value_2 |> Ok
        | _ -> Error "Notification transition time is not a number."

    let max_history_length_result_1 =
        match Int32.TryParse new_max_history_length_value_1.value with
        | true, new_value_1 ->
            let new_value_2 = new_value_1 |> LanguagePrimitives.Int32WithMeasure
            if new_value_2 < min_max_history_length then
                Error $"Maximum history length is too low (minimum {min_max_history_length})."
            elif new_value_2 > max_max_history_length then
                Error $"Maximum history length is too high (maximum {max_max_history_length})."
            else new_value_2 |> Ok
        | _ -> Error "Maximum history length is not a number."

    let mouse_wheel_action_elapsed_time_threshold_result_1 =
        match Int32.TryParse new_mouse_wheel_action_elapsed_time_threshold_value_1.value with
        | true, new_value_1 ->
            let new_value_2 = new_value_1 |> LanguagePrimitives.Int32WithMeasure
            if new_value_2 < min_mouse_wheel_action_elapsed_time_threshold then
                Error $"Mouse wheel action elapsed time threshold is too low (minimum {min_mouse_wheel_action_elapsed_time_threshold})."
            elif new_value_2 > max_mouse_wheel_action_elapsed_time_threshold then
                Error $"Mouse wheel action elapsed time threshold is too high (maximum {max_mouse_wheel_action_elapsed_time_threshold})."
            else new_value_2 |> Ok
        | _ -> Error "Mouse wheel action elapsed time threshold is not a number."

    match dialogue_typing_speed_result_1, notification_display_time_result_1, notification_transition_time_result_1, max_history_length_result_1, mouse_wheel_action_elapsed_time_threshold_result_1, key_bindings_result_1 with
    | Ok dialogue_typing_speed_result_2,
        Ok notification_display_time_result_2,
        Ok notification_transition_time_result_2,
        Ok max_history_length_result_2,
        Ok mouse_wheel_action_elapsed_time_threshold_result_2,
        Ok key_bindings_result_2 ->

        do configuration.current <-
            { configuration.current with
                dialogue_box_configuration =
                    { configuration.current.dialogue_box_configuration with
                        typing_speed = dialogue_typing_speed_result_2
                    }
                temporary_notifications_configuration =
                    { configuration.current.temporary_notifications_configuration with
                        display_time = notification_display_time_result_2
                        transition_time = notification_transition_time_result_2
                    }
                history_configuration =
                    { configuration.current.history_configuration with
                        max_history_length = max_history_length_result_2
                    }
                mouse_configuration =
                    { configuration.current.mouse_configuration with
                        wheel_action_elapsed_time_threshold = mouse_wheel_action_elapsed_time_threshold_result_2
                    }
                key_bindings_configuration = key_bindings_result_2
            }
        Ok ()

    | _ ->
        let key_bindings_result_2 =
            match key_bindings_result_1 with
            | Ok _ -> []
            | Error errors -> errors |> List.map box

        collect_errors_boxed ([box dialogue_typing_speed_result_1; box notification_display_time_result_1; box notification_transition_time_result_1; box max_history_length_result_1; box mouse_wheel_action_elapsed_time_threshold_result_1] @ key_bindings_result_2)

let handle_save_button_click
    (configuration : IRefValue<Runner_Configuration>)
    (set_configuration : Runner_Configuration -> unit)
    (dispatch : Configuration_Message -> unit)
    : unit =

(* If the validation succeeds, this function modifies the configuration. *)
    let validation_result =
        validate_configuration
            configuration
            (document.getElementById "txt_typing_speed" :?> HTMLTextAreaElement) 
            (document.getElementById "txt_mouse_wheel_action_elapsed_time_threshold" :?> HTMLTextAreaElement)
            (document.getElementById "txt_notification_display_time" :?> HTMLTextAreaElement)
            (document.getElementById "txt_notification_transition_time" :?> HTMLTextAreaElement)
            (document.getElementById "txt_max_history_length" :?> HTMLTextAreaElement)

    match validation_result with
    | Ok () ->
        do
            set_configuration_in_local_storage configuration
(* This propagates the configuration changes to the individual components. *)
            set_configuration configuration.current
            dispatch Hide
    | Error error_messages ->
        warn "configuration" true $"Configuration validation failed.{Environment.NewLine}{String.Join (Environment.NewLine, error_messages)}" []
