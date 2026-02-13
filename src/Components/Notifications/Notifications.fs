module Notifications

// Environment.NewLine
open System

// console, window
open Browser.Dom
// jsNative
open Fable.Core
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open JavaScript_Interop_1
open Log
open Notification_Component
open Notification_Types
open Temporary_Notification
open Transition
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Notifications"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

(* Types *)

type private Temporary_Notifications_Queue = Notification_Data_2 list

(* Global values *)

let mutable private temporary_notification_queue_lock = 0

(* Consts *)

let private game_paused_notification = "Game paused. Click to continue."

(* Main functions - state *)

let private notify_remove_temporary_notification_from_queue
    (queue : IRefValue<Temporary_Notifications_Queue>)
    (show : Notification_Data_2 -> unit)
    : unit =

    do lock (temporary_notification_queue_lock :> obj) (fun () ->
        match queue.current with
        | _ :: tail ->
            queue.current <- tail
            match queue.current with
            | head :: _ -> show head
            | [] -> ()
        | [] -> error "notify_remove_temporary_notification_from_queue" "Received notification to remove temporary notification from queue, but queue was already empty." [] |> invalidOp
    )

(* Main functions - render *)

let view
    (is_visible : bool)
    (permanent_notification_data_ref : IRefValue<Transition_State<Notification_State, Notification_Transition_Type>>)
    (set_permanent_notification_data_after_js_eval : Transition_State<Notification_State, Notification_Transition_Type> -> unit)
    (temporary_notification_component_ref : IRefValue<ReactElement>)
    : ReactElement =

    if is_visible then
        Html.div [
            prop.id "notifications_container"
            prop.style [style.zIndex notifications_z_index]
            prop.children [
                Html.div [
                    prop.id "permanent_notification_container_outer"
                    prop.children [
                        yield! permanent_notification_view permanent_notification_data_ref set_permanent_notification_data_after_js_eval
                    ]
                ]
                Html.div [
                    prop.id "temporary_notification_container_outer"
                    prop.children [
                        yield temporary_notification_component_ref.current
                    ]
                ]
            ]
        ]
    else Html.none

(* Main functions - interface *)

let private add_temporary_notification
    (queue : IRefValue<Temporary_Notifications_Queue>)
    (show : Notification_Data_2 -> unit)
    (data : Notification_Data_2)
    : unit =

    do lock (temporary_notification_queue_lock :> obj) (fun () ->
        match queue.current with
        | [] ->
            do
                queue.current <- [data]
                show data
        | _ -> do queue.current <- List.append queue.current [data]
    )

let private update_permanent_notification
    (configuration_ref : IRefValue<Notifications_Configuration>)
    (permanent_notification_data_before_js_eval_ref : IRefValue<string option>)
    (permanent_notification_data_after_eval_js_ref : IRefValue<Transition_State<Notification_State, Notification_Transition_Type>>)
    (set_permanent_notification_data_after_js_eval : Transition_State<Notification_State, Notification_Transition_Type> -> unit)
    (menu_variables : Menu_Variables)
    : unit =

    match permanent_notification_data_before_js_eval_ref.current with
    | None -> ()
    | Some value_1 ->

(* I_Temporary_Notification.show () is gated by the temporary notification queue, which does not change the temporary notification value until the fade out transition for the current value is done. However, the author can set the permanent notification at any time. If this happens during a transition, we just update the transition data to reflect the new value. The permanent notification cross fade transition completion handler does not notify the command queue, so we can interrupt the transition with no problem. The permanent notification cross fade transition completion handler just sets the permanent notification state back to Idle. If we interrupt the transition, it redraws the component, which just replaces the handler with a new one that does the same thing.
*)
        let old_data =
            match permanent_notification_data_after_eval_js_ref.current with
            | Idle old_data -> old_data
            | In_Transition transition_data -> transition_data.new_data

(* Previously, we called try_eval_js_with_menu_variables (), which, unlike try_eval_js_with_menu_variables (), does not raise an exception if the JavaScript code fails and returns null, but instead returns None. This was because the start script might run JavaScript functions, which would trigger an evaluation of the permanent notification text, which might contain JavaScript expressions with values that were not defined yet. As a result, trying to evalate those expressions would fail. This should not be an issue now. We do not evaluate the permanent notification text until the author first sets it. By that point, they should have defined all JavaScript values they intend to use.
*)
        let value_2 = eval_js_with_menu_variables<string> value_1 menu_variables
        let new_data = Visible { text = value_2 }

(* This comparison is valid because old_data and new_data are type Notification_State, which, in case Visible, contains Notification_Data_2, which contains the notification text after evaluating any JavaScript expressions in it. *)
        if old_data <> new_data then
            do set_permanent_notification_data_after_js_eval <| In_Transition {
                old_data = old_data
                new_data = new_data
                transition_type = Fade
                transition_time = configuration_ref.current.transition_time
(* See comments in handle_fade_in_or_fade_out_complete (). *)
                command_queue_item_id = 0<command_queue_item_id>
            }

(* Component *)

[<ReactComponent>]
let Notifications (
    props : {| expose : IRefValue<I_Notifications> |},
    initial_configuration : Notifications_Configuration
) : ReactElement =

(* State *)

(* We store the configuration in an IRefValue so the player can update it. *)
    let configuration_ref = React.useRef initial_configuration

    let is_visible, set_is_visible = React.useState true

    let queue : IRefValue<Temporary_Notifications_Queue> = React.useRef []

(* We store the permanent notification data both before and after we evaluate any JavaScript expressions in it. That way, if the author runs a JavaScript_Inline or JavaScript_Block command, we can re-evaluate the permanent notification data to reflect any changes the author made to JavaScript values.
*)
    let permanent_notification_data_before_eval_js_ref = React.useRef None
(* We use useState because we want this component to redraw itself whenever this value changes. *)
    let permanent_notification_data_after_eval_js, set_permanent_notification_data_after_js_eval = React.useState (Idle Hidden)
    let permanent_notification_data_after_eval_js_ref = React.useRef permanent_notification_data_after_eval_js
    do permanent_notification_data_after_eval_js_ref.current <- permanent_notification_data_after_eval_js

(* We need to pass a method from this interface (I_Temporary_Notification.show ()) to a callback (notify_remove_temporary_notification_from_queue) which we need to instantiate the component (Temporary_Notification_Component) that provides the interface. Therefore we use an IRefValue for the interface so we can "use" it before we have a working reference for it. *)
    let temporary_notification_interface : IRefValue<I_Temporary_Notification> = React.useRef <| Unchecked.defaultof<_>
    let temporary_notification_component = React.useRef <| Temporary_Notification_Component ({| expose = temporary_notification_interface |}, (fun () -> notify_remove_temporary_notification_from_queue queue temporary_notification_interface.current.show), initial_configuration)

(* Interface *)

    do React.useImperativeHandle (props.expose, fun () ->
        {
            new I_Notifications with
                member _.add_temporary_notification (data : Notification_Data_2) : unit =
                    add_temporary_notification queue temporary_notification_interface.current.show data

                member _.set_permanent_notification (data : string) (menu_variables : Menu_Variables) : unit =
                    do
                        permanent_notification_data_before_eval_js_ref.current <- Some data
                        update_permanent_notification configuration_ref permanent_notification_data_before_eval_js_ref permanent_notification_data_after_eval_js_ref set_permanent_notification_data_after_js_eval menu_variables

                member _.update_permanent_notification (menu_variables : Menu_Variables) : unit =
                    do update_permanent_notification configuration_ref permanent_notification_data_before_eval_js_ref permanent_notification_data_after_eval_js_ref set_permanent_notification_data_after_js_eval menu_variables

(* TODO2 #notification If we back up and re-do a notify command, the temporary notification does not appear again, and the stale label remains in the content tree instead of being removed. This only seems to happen when we change the code or CSS and cause Fable to recompile on the fly, though. So far we have not reproduced it in normal conditions. *)
                member _.get_state () : Notifications_Saveable_State =
                    {
                        permanent_notification_before_eval_js = permanent_notification_data_before_eval_js_ref.current
                        permanent_notification_after_eval_js =
                            match permanent_notification_data_after_eval_js_ref.current with
                            | Idle data -> data
                            | In_Transition transition_data -> transition_data.new_data
                    }
                member _.set_state (data : Notifications_Saveable_State) : unit =
                    do
                        permanent_notification_data_before_eval_js_ref.current <- data.permanent_notification_before_eval_js
                        set_permanent_notification_data_after_js_eval <| Idle data.permanent_notification_after_eval_js
(* We do not use this for now. *)
//                member _.get_configuration () : Temporary_Notifications_Configuration = configuration_ref.current
                member _.set_configuration (configuration : Notifications_Configuration) : unit =
                    do
                        configuration_ref.current <- configuration
                        temporary_notification_interface.current.set_configuration configuration
                member _.is_visible () : bool = is_visible
// TODO2 For permanent_notification_data_after_eval_js, Hidden just means "empty", so it does not correspond to is_visible = false.
                member _.show () : unit = set_is_visible true
                member _.hide () : unit = set_is_visible false
                member _.show_game_paused_notification () : unit = add_temporary_notification queue temporary_notification_interface.current.show { text = game_paused_notification }
        }
    )
(* This component does not implement I_Transitionable. *)

(* Render permanent notification. *)
    view is_visible permanent_notification_data_after_eval_js_ref set_permanent_notification_data_after_js_eval temporary_notification_component
