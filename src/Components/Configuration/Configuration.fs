module Configuration

// DateTime
open System

// HTMLElement
open Browser.Types
// Cmd
open Elmish
// ? operator
open Fable.Core.JsInterop
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz
// useElmish
open Feliz.UseElmish

open Configuration_Helpers
open Configuration_Types
open Key_Bindings
open Log
open Notification_Types
open Utilities

(* Debug *)

let debug_module_name = "Configuration"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

let private initial_state = {
    is_visible = false
}

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
            prop.className "interface_layer"
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
// TODO1 #configuration Replace numeric settings with slider components.
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
                        Html.h4 "Mouse"
                        Html.label [
                            prop.text $"Mouse wheel action elapsed time threshold (milliseconds, lower = faster mouse wheel scrolling, minimum {min_mouse_wheel_action_elapsed_time_threshold}, maximum {max_mouse_wheel_action_elapsed_time_threshold}, default {default_mouse_wheel_action_elapsed_time_threshold}): "
                        ]
                        Html.input [
                            prop.id "txt_mouse_wheel_action_elapsed_time_threshold"
                            prop.type' "text"
                            prop.maxLength 4
                            prop.style [style.width (length.em 5)]
                            prop.defaultValue (configuration.current.mouse_configuration.wheel_action_elapsed_time_threshold |> int)
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
                                    prop.text $"Notification fade in/fade out time (seconds, minimum {int min_notification_transition_time}, maximium {int max_notification_transition_time}): "
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
                        Html.h4 "History"
                        Html.div [
                            prop.className "configuration_grid"
                            prop.children [
                                Html.label [
                                    prop.text $"Maximum undo/redo history length (minimum {min_max_history_length}, maximum {max_max_history_length}, 0 = unlimited): "
                                ]
                                Html.input [
                                    prop.id "txt_max_history_length"
                                    prop.type' "text"
                                    prop.maxLength 2
                                    prop.style [style.width (length.em 3)]
                                    prop.defaultValue configuration.current.history_configuration.max_history_length
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
                                    handle_save_button_click configuration set_configuration dispatch
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
    (show_pause_notification : unit -> unit)
    (message : Configuration_Message)
    (state : Configuration_State)
    : Configuration_State * Cmd<Configuration_Message> =

    match message with

    | Show ->
        { state with is_visible = true }, Cmd.none

    | Hide ->
        do show_pause_notification ()
        { state with is_visible = false }, Cmd.none

(* Component *)

[<ReactComponent>]
let Configuration
    (props : {| expose : IRefValue<I_Configuration> |},
    configuration : IRefValue<Runner_Configuration>,
(* We cannot simply change the configuration here. We must propagate the changes to the individual components, which is what set_configuration () does. The configuration cannot contain references to individual components because we must serialize and deserialize it, and we would have to serialize and deserialize the individual component configurations.
*)
    set_configuration : Runner_Configuration -> unit,
    show_pause_notification : unit -> unit,
    redraw_command_menu : unit -> unit
    )
    : ReactElement =

(* State *)

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update show_pause_notification, [||])
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
