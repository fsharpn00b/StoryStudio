module Temporary_Notification

// console, window
open Browser.Dom
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Log
open Notification_Component
open Notification_Types
open Transition
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Temporary_Notification"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

(* Main functions - state *)

let private handle_fade_in_or_fade_out_complete
    (fade_state_ref : IRefValue<Transition_State<Notification_State, Notification_Transition_Type>>)
    (set_fade_state : Transition_State<Notification_State, Notification_Transition_Type> -> unit)
    (configuration : IRefValue<Notifications_Configuration>)
    (notify_queue : unit -> unit)
(* This is not a valid Runner_Queue item ID. A component that supports fade transitions typically notifies Runner_Queue when a transition is complete, so Runner_Queue can run the next command. In this case, we have the Temporary_Notification component notify the Temporary_Notifications_Queue instead. *)
    (fade_in_or_fade_out : int<command_queue_item_id>)
    : unit =

    if notify_fade_in_complete = fade_in_or_fade_out then
        let new_data =
            match fade_state_ref.current with
            | In_Transition transition_data -> transition_data.new_data
            | _ -> error "handle_fade_in_or_fade_out_complete" "Unexpected state. Expected In_Transition." ["state", fade_state_ref.current] |> invalidOp

        set_fade_state <| Idle new_data

// TODO2 #notification Add timeout function handles to cancel notification transitions?

        window.setTimeout ((fun () ->
            set_fade_state <| In_Transition {
                old_data = new_data
                new_data = Hidden
                transition_type = Fade
                transition_time = configuration.current.transition_time
(* See comments in handle_fade_in_or_fade_out_complete (). *)
                command_queue_item_id = notify_fade_out_complete
            }
        ), int configuration.current.display_time * 1000) |> ignore

    elif notify_fade_out_complete = fade_in_or_fade_out then
        set_fade_state <| Idle Hidden
        window.setTimeout (notify_queue, int notify_transition_complete_delay_time) |> ignore

    else error "handle_fade_in_or_fade_out_complete" "Unexpected fade in/fade out notification. Expected 0 (fade in) or 1 (fade out)." ["fade_in_or_fade_out", int fade_in_or_fade_out] |> invalidOp

(* Component *)

[<ReactComponent>]
let Temporary_Notification_Component
    (props : {| expose : IRefValue<I_Temporary_Notification> |},
    notify_queue : unit -> unit,
    initial_configuration : Notifications_Configuration
    )
    : ReactElement =

(* State *)

(* We store the configuration in an IRefValue so the player can update it. *)
    let configuration_ref = React.useRef initial_configuration

// TODO2 This currently does nothing, so we pass in a dummy implementation.
    let fade_configuration : Transition_Configuration = ()
(* This component does not notify Runner_Queue when it completes a transition. Instead, it notifies Temporary_Notifications_Queue by calling handle_fade_in_or_fade_out_complete (). *)
    let fade_state, set_fade_state = React.useState (Idle Hidden)
    let fade_state_ref = React.useRef fade_state
    do fade_state_ref.current <- fade_state

(* Partially apply this function so we can pass it as a callback to the Temporary_Notification_Component constructor. *)
    let handle_fade_in_or_fade_out_complete_2 =
        handle_fade_in_or_fade_out_complete fade_state_ref set_fade_state configuration_ref notify_queue

    do React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Temporary_Notification with
                member _.show
                    (data : Notification_Data_2)
                    : unit =

                    let old_data =
                        match fade_state_ref.current with
                        | Idle old_data -> old_data
                        | _ -> error "show" "Unexpected state. Expected Idle." ["state", fade_state_ref.current] |> invalidOp

                    set_fade_state <| In_Transition {
                        old_data = old_data
                        new_data = Visible data
                        transition_type = Fade
                        transition_time = configuration_ref.current.transition_time
(* See comments in handle_fade_in_or_fade_out_complete (). *)
                        command_queue_item_id = notify_fade_in_complete
                    }

                member _.set_configuration (configuration : Notifications_Configuration) : unit =
                    do configuration_ref.current <- configuration
        }
    )
(* This component does not implement I_Transitionable. *)

    temporary_notifications_view fade_state_ref handle_fade_in_or_fade_out_complete_2
