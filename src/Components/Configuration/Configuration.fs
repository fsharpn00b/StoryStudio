module Configuration

// DateTime
open System

// navigator, Types (also provides console, window)
open Browser
// a, Element, HTMLCanvasElement, HTMLElement, HTMLTextAreaElement
open Browser.Types
// Cmd
open Elmish
// ? operator
open Fable.Core.JsInterop
// Html, IRefValue, React, React, ReactComponent, ReactElement
open Feliz
// useElmish
open Feliz.UseElmish
// Decode, Encode
open Thoth.Json

open Background
open Character_Types
open Dialogue_Box_Types
open Key_Bindings
open Log
open Temporary_Notification
open Units_Of_Measure
open Utilities

(* Debug *)

let debug_module_name = "Configuration"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

type Runner_Configuration = {
(* These fields cannot be IRefValue because we need to serialize this type. *)
    background_configuration : Background_Configuration
    characters_configuration : Characters_Configuration
    dialogue_box_configuration : Dialogue_Box_Configuration
    temporary_notifications_configuration : Temporary_Notifications_Configuration
    key_bindings_configuration : Key_Bindings_Configuration
}

type private Configuration_State = {
    is_visible : bool
}

type private Configuration_Message =
    | Show
    | Hide

(* Interfaces *)

type I_Configuration =
    abstract member show : unit -> unit
    abstract member hide : unit -> unit
    abstract member is_visible : unit -> bool

(* Consts *)

let private local_storage_name = "vnf_configuration"

let private initial_state = {
    is_visible = false
}

(* Helper functions *)

let private characters_per_second_to_delay_between_characters
    (characters_per_second : int)
    : int<milliseconds>
    =
    int (1000 / characters_per_second) |> LanguagePrimitives.Int32WithMeasure

let private delay_between_characters_to_characters_per_second
    (delay_between_characters : int)
    : int =
    int (1000 / delay_between_characters)

let get_configuration_from_local_storage () : Runner_Configuration option =
    match localStorage.getItem local_storage_name with
    | null -> None
    | json ->
        match Decode.Auto.fromString<Runner_Configuration> json with
        | Ok configuration -> Some configuration
        | _ -> error "get_configuration_from_local_storage" "Failed to deserialize configuration." ["json", json] |> invalidOp

let private set_configuration_in_local_storage
    (configuration : IRefValue<Runner_Configuration>) : unit =
    let json = Encode.Auto.toString (0, configuration.current)
    do localStorage.setItem (local_storage_name, json)

let private update_configuration
    (configuration : IRefValue<Runner_Configuration>)
    (set_configuration : Runner_Configuration -> unit)
    (new_typing_speed_value_1 : HTMLTextAreaElement)
    (new_notification_display_time_value_1 : HTMLTextAreaElement)
    (new_notification_transition_time_value_1 : HTMLTextAreaElement)
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
            if new_value_2 < int min_temporary_notification_transition_time then int min_temporary_notification_transition_time
            elif new_value_2 > int max_temporary_notification_transition_time then int max_temporary_notification_transition_time
            else new_value_2

        do configuration.current <- { configuration.current with temporary_notifications_configuration = { configuration.current.temporary_notifications_configuration with transition_time = new_value_3 |> float |> LanguagePrimitives.FloatWithMeasure } }
    | _ -> ()

    do configuration.current <- { configuration.current with key_bindings_configuration = new_key_bindings_configuration }

    set_configuration configuration.current
    set_configuration_in_local_storage configuration

(* Main functions - rendering *)

let private view
    (configuration : IRefValue<Runner_Configuration>)
    (set_configuration : Runner_Configuration -> unit)
    (element_ref : IRefValue<HTMLElement option>)
    (state : IRefValue<Configuration_State>)
    (dispatch : Configuration_Message -> unit)
    : ReactElement =

    if state.current.is_visible then
        Html.div [
(* Make sure this screen can receive focus. *)
            prop.ref element_ref
            prop.tabIndex 0
            prop.onKeyDown (fun event ->
                do event.stopPropagation ()
(* This is only if we want to block default browser behavior. *)
//                event.preventDefault ()
(* Ignore all key down events except Escape, which cannot be re-bound anyway. We do not want the player to mistakenly exit this screen while trying to enter a key binding. *)
                if 0 = String.Compare ("Escape", event.key) then do dispatch Hide 
            )
(* Prevent a mouse click from calling Runner.run (). *)
            prop.onClick (fun event -> do event.stopPropagation ())
(* Prevent a mouse wheel scroll event from calling Runner.undo ()/redo (). *)
            prop.onWheel (fun event -> do event.stopPropagation ())

            prop.id "configuration_screen"
            prop.style [style.zIndex configuration_z_index]
            prop.children [
                Html.div [
                    prop.className "configuration_header"
                    prop.children [
                        Html.h3 [
                            prop.text $"Configuration (Escape to exit)"
                        ]
                    ]
                ]
                Html.div [
                    prop.className "configuration_items"
                    prop.children [
                        Html.h4 "Dialogue"
                        Html.label [
                            prop.text "Dialogue typing speed (characters per second, 0 = show all at once): "
                        ]
                        Html.input [
                            prop.id "txt_typing_speed"
(* It seems we are supposed to use this instead of ``type``. *)
                            prop.type' "text"
                            prop.maxLength 3
                            prop.style [style.width (length.em 4)]
                            prop.defaultValue (configuration.current.dialogue_box_configuration.typing_speed |> int |> delay_between_characters_to_characters_per_second)
                        ]
                        Html.h4 "Notifications"
                        Html.div [
                            prop.className "configuration_grid"
                            prop.children [
                                Html.label [
                                    prop.text $"Notification display time (seconds, minimum {int min_temporary_notification_display_time}, maximum {int max_temporary_notification_display_time}): "
                                ]
                                Html.input [
                                    prop.id "txt_notification_display_time"
                                    prop.type' "text"
                                    prop.maxLength 2
                                    prop.style [style.width (length.em 3)]
                                    prop.defaultValue (configuration.current.temporary_notifications_configuration.display_time |> int)
                                ]

                                Html.label [
                                    prop.text $"Notification fade in/fade out time (seconds, minimum {int min_temporary_notification_transition_time}, maximium {int max_temporary_notification_transition_time}): "
                                ]
                                Html.input [
                                    prop.id "txt_notification_transition_time"
                                    prop.type' "text"
                                    prop.maxLength 2
                                    prop.style [style.width (length.em 3)]
                                    prop.defaultValue (configuration.current.temporary_notifications_configuration.transition_time |> int)
                                ]
                            ]
                        ]
                        Html.h4 "Key bindings"
                        Html.div [
                            prop.className "configuration_grid"
                            prop.children [
                                yield! get_key_binding_elements configuration.current.key_bindings_configuration
                            ]
                        ]
                    ]
                ]
                Html.div [
                    prop.className "controls"
                    prop.children [
                        Html.button [
                            prop.text "Save"
                            prop.onClick (fun event ->
                                do
                                    event.stopPropagation ()
                                    match get_key_bindings_configuration () with
                                    | None -> ()
                                    | Some key_bindings_configuration ->
                                        update_configuration configuration set_configuration (document.getElementById "txt_typing_speed" :?> HTMLTextAreaElement) (document.getElementById "txt_notification_display_time" :?> HTMLTextAreaElement) (document.getElementById "txt_notification_transition_time" :?> HTMLTextAreaElement) key_bindings_configuration
                                        dispatch Hide
                            )
                        ]
                        Html.button [
                            prop.text "Exit"
                            prop.onClick (fun event ->
                                do
                                    event.stopPropagation ()
                                    dispatch Hide
                            )
                        ]
                    ]
                ]
            ]
        ]
    else Html.none

(* Main functions - state *)

let private update
    (show_game_paused_notification : unit -> unit)
    (message : Configuration_Message)
    (state : Configuration_State)
    : Configuration_State * Cmd<Configuration_Message> =

    match message with

    | Show ->
        {
            state with
                is_visible = true
        }, Cmd.none

    | Hide ->
        do show_game_paused_notification ()
        { state with is_visible = false }, Cmd.none

(* Component *)

[<ReactComponent>]
let Configuration
    (props : {| expose : IRefValue<I_Configuration> |},
    configuration : IRefValue<Runner_Configuration>,
(* We cannot simply change the configuration here. We must propagate the changes to the individual components, which is what set_configuration () does. The configuration cannot contain references to individual components because we must serialize and deserialize it, and we would have to serialize and deserialize the individual component configurations.
*)
    set_configuration : Runner_Configuration -> unit,
    show_game_paused_notification : unit -> unit,
    redraw_command_menu : unit -> unit
    )
    : ReactElement =

(* State *)

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update show_game_paused_notification, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

(* Give focus to this component when it is visible. This is so we can prevent mouse click and key down events leaking to the game. *)
    let element_ref = React.useRef None
    React.useEffect ((fun () -> if state.is_visible then element_ref.current?focus()), [| box state.is_visible |])

(* Notify the command menu when this screen shows or hides itself, so the command menu can enable or disable the appropriate commands. *)
    React.useEffect ((fun () -> redraw_command_menu ()), [| box state.is_visible |])

(* Interface *)

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Configuration with
(* The configuration screen does not need to notify transition complete. It does not have a Command_Behavior, and should not add to the history or call get_next_command ().
*)
                member _.show () = dispatch <| Show
                member _.hide () = dispatch <| Hide
                member _.is_visible (): bool = state_ref.current.is_visible
        }
    )

(* Render *)

    view configuration set_configuration element_ref state_ref dispatch
