module Temporary_Notification

// console, window
open Browser.Dom
// Cmd
open Elmish
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz
// useElmish
open Feliz.UseElmish

open Log
open Transition
open Transition_Types
open Units_Of_Measure

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

let private view_fade_in_out
    (is_pre_transition : bool)
    (is_fade_in : bool)
    (data : Notification_Data_2)
    (transition_time : Transition_Time)
    : ReactElement =

    let opacity =
        match is_fade_in, is_pre_transition with
        | true, true -> 0.0
        | true, false -> 1.0
        | false, true -> 1.0
        | false, false -> 0.0

    Html.label [
        prop.key data.text
        prop.className "notification"
        prop.text data.text
        prop.style [
            style.opacity opacity
            style.custom ("transition", $"opacity {transition_time}s ease-in-out")
        ]
    ]

let private view_cross_fade
    (is_pre_transition : bool)
    (old_data : Notification_Data_2)
    (new_data : Notification_Data_2)
    (transition_time : Transition_Time)
    : ReactElement seq =

    [
        Html.label [
            prop.key old_data.text
            prop.className "notification"
            prop.text old_data.text
            prop.style [
                style.opacity <| if is_pre_transition then 1.0 else 0.0
                style.custom ("transition", $"opacity {transition_time}s ease-in-out")
            ]
        ]
        Html.label [
            prop.key new_data.text
            prop.className "notification"
            prop.text new_data.text
            prop.style [
                style.opacity <| if is_pre_transition then 0.0 else 1.0
                style.custom ("transition", $"opacity {transition_time}s ease-in-out")
            ]
        ]
    ]

let private view
    (fade_state : IRefValue<Transition_State<Notification_State, Notification_Transition_Type>>)
    : ReactElement =

    Html.div [
        match fade_state.current with
        | Idle Hidden -> Html.none
        | Idle (Visible data) -> view_idle_visible data
        | Pre_Transition transition_data ->
            match transition_data.old_data, transition_data.new_data with
            | Hidden, Visible data -> view_fade_in_out true true data transition_data.transition_time
            | Visible data, Hidden -> view_fade_in_out true false data transition_data.transition_time
            | Visible old_data, Visible new_data -> yield! view_cross_fade true old_data new_data transition_data.transition_time
            | _ -> error "view_2" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp
        | In_Transition transition_data ->
            match transition_data.old_data, transition_data.new_data with
            | Hidden, Visible data -> view_fade_in_out false true data transition_data.transition_time
            | Visible data, Hidden -> view_fade_in_out false false data transition_data.transition_time
            | Visible old_data, Visible new_data -> yield! view_cross_fade false old_data new_data transition_data.transition_time
            | _ -> error "view_2" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp
    ]

(* Main functions - state *)

let private notify_fade_in_or_fade_out_complete
    (dispatch : IRefValue<Transition_Message<Notification_State, Notification_Transition_Type> -> unit>)
    (configuration : IRefValue<Temporary_Notifications_Configuration>)
    (notify_queue : unit -> unit)
(* This is not a valid Runner_Queue item ID. A component that supports fade transitions typically notifies Runner_Queue when a transition is complete, so Runner_Queue can run the next command. In this case, we have the Temporary_Notification component notify the Temporary_Notifications_Queue instead. *)
    (fade_in_or_fade_out : int<runner_queue_item_id>)
    : unit =

    if notify_fade_in_complete = fade_in_or_fade_out then
        window.setTimeout ((fun () ->
            dispatch.current (Transition {
                transition_type = Fade
                new_data = Hidden
                transition_time = configuration.current.transition_time
                command_queue_item_id = notify_fade_out_complete
            })
        ), int configuration.current.display_time * 1000) |> ignore
    elif notify_fade_out_complete = fade_in_or_fade_out then
        notify_queue ()

    else error "notify_fade_in_or_fade_out_complete" "Unexpected fade in/fade out notification. Expected 0 (fade in) or 1 (fade out)." ["fade_in_or_fade_out", int fade_in_or_fade_out] |> invalidOp

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

(* We need to pass the dispatch () function exposed by Temporary_Notification_Component to a callback (notify_fade_in_or_fade_out_complete ()) which we also need to instantiate the Temporary_Notification_Component. Therefore we use an IRefValue for the dispatch () function so we can "use" it before we have a working reference for it. *)
    let dispatch_ref = React.useRef Unchecked.defaultof<_>
(* Partially apply this function so we can pass it as a callback to the Temporary_Notification_Component constructor. *)
    let notify_fade_in_or_fade_out_complete (fade_in_or_fade_out : int<runner_queue_item_id>) : unit = notify_fade_in_or_fade_out_complete dispatch_ref configuration_ref notify_queue fade_in_or_fade_out

// TODO2 This currently does nothing, so we pass in a dummy implementation.
    let fade_configuration : Transition_Configuration = ()
    let Transition_Timeout_function_handle = React.useRef None
(* This component does not notify Runner_Queue when it completes a transition. Instead, it notifies Temporary_Notifications_Queue by calling notify_fade_in_or_fade_out_complete (). *)
    let fade_state, dispatch = React.useElmish((Idle Hidden, Cmd.none), update fade_configuration Transition_Timeout_function_handle notify_fade_in_or_fade_out_complete, [||])
    let fade_state_ref = React.useRef fade_state
    do fade_state_ref.current <- fade_state
(* Save the dispatch () function exposed by Temporary_Notification_Component. See comments where this value is declared. *)
    do dispatch_ref.current <- dispatch

    do React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Temporary_Notification with
                member _.show
                    (data : Notification_Data_2)
                    : unit =
                    do dispatch (Transition {
                        transition_type = Fade
                        new_data = Visible data
                        transition_time = configuration_ref.current.transition_time
(* See comments in notify_fade_in_or_fade_out_complete (). *)
                        command_queue_item_id = notify_fade_in_complete
                    })
                member _.set_configuration (configuration : Temporary_Notifications_Configuration) : unit =
                    do configuration_ref.current <- configuration
        }
    )
(* This component does not implement I_Transitionable. *)

    view fade_state_ref
