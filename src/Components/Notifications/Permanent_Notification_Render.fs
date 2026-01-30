module Permanent_Notification_Render

// Environment.NewLine
open System

// console, window
open Browser.Dom
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Log
open Temporary_Notification
open Transition
open Utilities

(* Debug *)

let private log_module_name = "Permanent_Notification_Render"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

(* Functions - rendering *)

let permanent_notification_view
    (is_visible : bool)
    (permanent_notification_data_ref : IRefValue<Transition_State<Notification_State, Notification_Transition_Type>>)
    (set_permanent_notification_data_after_js_eval : Transition_State<Notification_State, Notification_Transition_Type> -> unit)
    (temporary_notification_component : IRefValue<ReactElement>)
    : ReactElement =

    let complete_transition () =
        match permanent_notification_data_ref.current with
        | In_Transition transition_data -> set_permanent_notification_data_after_js_eval (Idle transition_data.new_data)
        | _ -> error "complete_transition" "Unexpected state. Expected In_Transition." ["state", permanent_notification_data_ref.current] |> invalidOp

    let class_name = "permanent_notification_label"

    if is_visible then
        Html.div [
            prop.id "notifications_container"
            prop.style [style.zIndex notifications_z_index]
            prop.children [
                Html.div [
                    prop.id "permanent_notification_container"
                    prop.children [
                        match permanent_notification_data_ref.current with

                        | Idle Hidden -> yield! []

                        | Idle (Visible data) -> view_idle_visible data class_name

                        | In_Transition transition_data ->

                            match transition_data.old_data, transition_data.new_data with

                            | Hidden, Visible data ->
                                Fade_Label (class_name, data, "0.0", "1.0", transition_data.transition_time, complete_transition)

                            | Visible old_data, Visible new_data ->
                                Fade_Label (class_name, old_data, "1.0", "0.0", transition_data.transition_time, complete_transition)
                                Fade_Label (class_name, new_data, "0.0", "1.0", transition_data.transition_time, complete_transition)

                            | _ -> error "view" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp
                    ]
                ]
                Html.div [
                    prop.id "temporary_notification_container"
                    prop.children [
                        temporary_notification_component.current
                    ]
                ]
            ]
        ]
    else Html.none
