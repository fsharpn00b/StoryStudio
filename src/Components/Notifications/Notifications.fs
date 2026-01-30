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
open Permanent_Notification_Render
open Temporary_Notification
open Transition
open Units_Of_Measure

(* Debug *)

let private log_module_name = "Notifications"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

(* Types *)

type Temporary_Notifications_Queue = Notification_Data_2 list

(* TODO2 For now, we only worry about permanent notifications.
If we decide to handle temporary notifications:
- Save timeout function handle to cancel transition/notification in case we load new game/undo?
- Also need to clear notification queue in that case?
*)
type Notifications_Saveable_State = {
    permanent_notification_before_eval_js : string option
    permanent_notification_after_eval_js : Notification_State
}

(* Interfaces *)

type I_Notifications =
    abstract member add_temporary_notification : Notification_Data_2 -> unit
    abstract member set_permanent_notification : string -> Menu_Variables -> unit
    abstract member update_permanent_notification : Menu_Variables -> unit
    abstract member get_state : unit -> Notifications_Saveable_State
    abstract member set_state : Notifications_Saveable_State -> unit
(* We do not use this for now. *)
//    abstract member get_configuration : unit -> Temporary_Notifications_Configuration
    abstract member set_configuration : Notifications_Configuration -> unit
    abstract member show : unit -> unit
    abstract member hide : unit -> unit
    abstract member is_visible : unit -> bool
(* When we pause the game by calling Runner_Transition.force_complete_transitions (), we want to show a temporary notification to tell the player the game is paused and they must click to continue.
x After we show the save/load screen.
x After we show the configuration screen.
x After the player imports/exports current/multiple saved games to/from file.
x Exceptions: Runner_Queue.run (), Runner_History.undo_redo ().
*)
    abstract member show_game_paused_notification : unit -> unit

(* Global values *)

let mutable temporary_notification_queue_lock = 0

(* Consts *)

let private game_paused_notification = "Game paused. Click to continue."

(* Main functions - state *)

let notify_remove_temporary_notification_from_queue
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

(* Main functions - interface *)

let add_temporary_notification
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

let update_permanent_notification
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

(* Previously, we called try_eval_js_string_with_menu_variables (). This was because the start script might run JavaScript functions, which would trigger an evaluation of the permanent notification text, which might contain JavaScript expressions with values that were not defined yet. As a result, trying to evalate those expressions would fail. This should not be an issue now. We do not evaluate the permanent notification text until the author first sets it. By that point, they should have defined all JavaScript values they intend to use.
*)
        let value_2 = eval_js_string_with_menu_variables value_1 menu_variables
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
                member _.add_temporary_notification (data : Notification_Data_2) : unit = add_temporary_notification queue temporary_notification_interface.current.show data

                member _.set_permanent_notification (data : string) (menu_variables : Menu_Variables) : unit =
                    do
                        permanent_notification_data_before_eval_js_ref.current <- Some data
                        update_permanent_notification configuration_ref permanent_notification_data_before_eval_js_ref permanent_notification_data_after_eval_js_ref set_permanent_notification_data_after_js_eval menu_variables

                member _.update_permanent_notification (menu_variables : Menu_Variables) : unit =
                    do update_permanent_notification configuration_ref permanent_notification_data_before_eval_js_ref permanent_notification_data_after_eval_js_ref set_permanent_notification_data_after_js_eval menu_variables

(* TODO1 #notification If we back up and re-do a notify command, the temporary notification does not appear again, and the stale label remains in the content tree instead of being. This only seems to happen if we change the code or CSS and Fable recompiles on the fly, though. *)
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
    permanent_notification_view is_visible permanent_notification_data_after_eval_js_ref set_permanent_notification_data_after_js_eval temporary_notification_component
