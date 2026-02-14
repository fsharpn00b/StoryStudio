module Notification_Component

// console, window
open Browser.Dom
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Log
open Notification_Types
open Transition
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Notification_Component"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

(* Main functions - rendering *)

let view_idle_visible
    (data : Notification_Data_2)
    (temporary_or_permanent : Notification_Type)
    : ReactElement =

    Html.div [
        prop.className (match temporary_or_permanent with | Temporary -> "temporary_notification_container_inner" | Permanent -> "permanent_notification_container_inner")

        prop.children [
            Html.label [
                prop.key data.text
                prop.className (match temporary_or_permanent with | Temporary -> "temporary_notification_label" | Permanent -> "permanent_notification_label")
                prop.text data.text
            ]
        ]
    ]

[<ReactComponent>]
let Fade_Label_Container (
    temporary_or_permanent : Notification_Type,
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

    Html.div [
        prop.className (match temporary_or_permanent with | Temporary -> "temporary_notification_container_inner" | Permanent -> "permanent_notification_container_inner")
        prop.style [
            style.custom ("opacity", opacity)
            style.custom ("transition", $"opacity {transition_time}s ease-in-out")
        ]
        prop.onTransitionEnd (fun _ ->
            handle_transition_end ()
        )
        prop.children [
            Html.label [
                prop.key data.text
                prop.className (match temporary_or_permanent with | Temporary -> "temporary_notification_label" | Permanent -> "permanent_notification_label")
                prop.text data.text
            ]
        ]
    ]

let temporary_notifications_view
    (fade_state : IRefValue<Transition_State<Notification_State, Notification_Transition_Type>>)
    (notify : int<command_queue_item_id> -> unit)
    : ReactElement =

    match fade_state.current with

    | Idle Hidden -> Html.none

    | Idle (Visible data) -> view_idle_visible data Temporary

    | In_Transition transition_data ->
        match transition_data.old_data, transition_data.new_data with

        | Hidden, Visible data ->
            Fade_Label_Container (Temporary, data, "0.0", "1.0", transition_data.transition_time, (fun () -> notify notify_fade_in_complete))

        | Visible data, Hidden ->
            Fade_Label_Container (Temporary, data, "1.0", "0.0", transition_data.transition_time, (fun () -> notify notify_fade_out_complete))

        | _ -> error "temporary_notifications_view" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp

let permanent_notification_view
    (permanent_notification_data_ref : IRefValue<Transition_State<Notification_State, Notification_Transition_Type>>)
    (set_permanent_notification_data_after_js_eval : Transition_State<Notification_State, Notification_Transition_Type> -> unit)
    : ReactElement seq =

    let complete_transition () =
        match permanent_notification_data_ref.current with
        | In_Transition transition_data -> set_permanent_notification_data_after_js_eval (Idle transition_data.new_data)
        | _ -> error "complete_transition" "Unexpected state. Expected In_Transition." ["state", permanent_notification_data_ref.current] |> invalidOp

    match permanent_notification_data_ref.current with

    | Idle Hidden -> Seq.empty

    | Idle (Visible data) -> view_idle_visible data Permanent |> Seq.singleton

    | In_Transition transition_data ->

        match transition_data.old_data, transition_data.new_data with

        | Hidden, Visible data ->
            Fade_Label_Container (Permanent, data, "0.0", "1.0", transition_data.transition_time, complete_transition) |> Seq.singleton

        | Visible old_data, Visible new_data ->
            [
                Fade_Label_Container (Permanent, old_data, "1.0", "0.0", transition_data.transition_time, complete_transition)
                Fade_Label_Container (Permanent, new_data, "0.0", "1.0", transition_data.transition_time, complete_transition)
            ]

        | Visible data, Hidden ->
            Fade_Label_Container (Permanent, data, "1.0", "0.0", transition_data.transition_time, complete_transition) |> Seq.singleton

        | _ -> error "permanent_notification_view" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp
