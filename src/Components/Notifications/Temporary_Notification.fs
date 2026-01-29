module Temporary_Notification

// console, window
open Browser.Dom
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Log
open Transition
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Temporary_Notification"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

(* Types *)

type Notification_Transition_Type = Fade

type Temporary_Notifications_Configuration = {
    display_time : Temporary_Notification_Display_Time
    transition_time : Transition_Time
}

type Notification_Data_1 = {
    text : string
    javascript_interpolations : string list
}

(* This is after we have applied JavaScript interpolations to the text field. *)
type Notification_Data_2 = {
    text : string
}

type Notification_State =
    | Visible of Notification_Data_2
    | Hidden

(* Interfaces *)

type I_Temporary_Notification =
    abstract member show : Notification_Data_2 -> unit
    abstract member set_configuration : Temporary_Notifications_Configuration -> unit

(* Consts *)

let max_temporary_notification_display_time = 30<seconds>
let min_temporary_notification_display_time = 1<seconds>
let max_temporary_notification_transition_time = 5<seconds>
let min_temporary_notification_transition_time = 0<seconds>

let private notify_fade_in_complete = 0<runner_queue_item_id>
let private notify_fade_out_complete = 1<runner_queue_item_id>

(* Main functions - rendering *)

let private view_idle_visible
    (data : Notification_Data_2)
    : ReactElement =

    Html.label [
        prop.key data.text
        prop.className "notification"
        prop.text data.text
    ]

[<ReactComponent>]
let Fade_Label (
    data : Notification_Data_2,
    initial_value : string,
    final_value : string,
    transition_time : Transition_Time,
    handle_transition_end : unit -> unit
    ) : ReactElement =

    let opacity, set_opacity = React.useState initial_value

    React.useEffectOnce (fun () ->
        window.setTimeout ((fun () -> set_opacity final_value), int pre_transition_time) |> ignore
    )

    Html.label [
        prop.key data.text
        prop.className "notification"
        prop.text data.text
        prop.style [
            style.custom ("opacity", opacity)
            style.custom ("transition", $"opacity {transition_time}s ease-in-out")
        ]
        prop.onTransitionEnd (fun _ ->
            handle_transition_end ()
        )
    ]

let private view
    (fade_state : IRefValue<Transition_State<Notification_State, Notification_Transition_Type>>)
    (notify : int<runner_queue_item_id> -> unit)
    : ReactElement =

    Html.div [
        match fade_state.current with
        | Idle Hidden -> Html.none
        | Idle (Visible data) -> view_idle_visible data
        | In_Transition transition_data ->
            match transition_data.old_data, transition_data.new_data with

            | Hidden, Visible data ->
                Fade_Label (data, "0.0", "1.0",  transition_data.transition_time, (fun () -> notify notify_fade_in_complete))

            | Visible data, Hidden ->
                Fade_Label (data, "1.0", "0.0", transition_data.transition_time, (fun () -> notify notify_fade_out_complete))

// TODO1 This is not used yet. We want to use it for permanent notification. Might not be needed here though. Permanent notification can have its own view function.
(*
            | Visible old_data, Visible new_data ->
                yield! [
                    Fade_Label (old_data, "1.0", "0.0", transition_data.transition_time, notify)
                    Fade_Label (new_data, "0.0", "1.0", transition_data.transition_time, notify)
                ]
*)

            | _ -> error "view" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp
    ]

(* Main functions - state *)

let private handle_fade_in_or_fade_out_complete
    (fade_state_ref : IRefValue<Transition_State<Notification_State, Notification_Transition_Type>>)
    (set_fade_state : Transition_State<Notification_State, Notification_Transition_Type> -> unit)
    (configuration : IRefValue<Temporary_Notifications_Configuration>)
    (notify_queue : unit -> unit)
(* This is not a valid Runner_Queue item ID. A component that supports fade transitions typically notifies Runner_Queue when a transition is complete, so Runner_Queue can run the next command. In this case, we have the Temporary_Notification component notify the Temporary_Notifications_Queue instead. *)
    (fade_in_or_fade_out : int<runner_queue_item_id>)
    : unit =

    if notify_fade_in_complete = fade_in_or_fade_out then
        let new_data =
            match fade_state_ref.current with
            | In_Transition transition_data -> transition_data.new_data
            | _ -> error "handle_fade_in_or_fade_out_complete" "Unexpected state. Expected In_Transition." ["state", fade_state_ref.current] |> invalidOp

        set_fade_state <| Idle new_data

// TODO1 Add timeout function handles to cancel notifications.

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
    initial_configuration : Temporary_Notifications_Configuration
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

                member _.set_configuration (configuration : Temporary_Notifications_Configuration) : unit =
                    do configuration_ref.current <- configuration
        }
    )
(* This component does not implement I_Transitionable. *)

    view fade_state_ref handle_fade_in_or_fade_out_complete_2
