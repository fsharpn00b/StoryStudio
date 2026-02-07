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

(* TODO1 #notifications Add the following animations:
1 When the permanent notification goes from Hidden to Visible or Visible to Hidden, or the temporary notification queue goes from empty to non-empty or from non-empty to empty, fade in or out the alpha value of the background blur.
2 When the permanent notification cross-fades, or the temporary notification fades out and a new temporary notification fades in, animate the width value of the corresponding label or label container so the background blur expands or contracts.

To do this, we would probably need to expand Fade_Label to include the label container, because we think that's where we should add the styling for the background blur, rather than add it to the label, though we might be wrong.

Example of style.width setting for label:
style.width (length.em (data.text.Length + 1))
(end)
*)

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
