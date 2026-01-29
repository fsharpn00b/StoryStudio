module Notifications

// Environment.NewLine
open System

// console, window
open Browser.Dom
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Log
open Temporary_Notification
open Utilities

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
    permanent_notification_before_eval_js : string
    permanent_notification_after_eval_js : string
}

(* Interfaces *)

type I_Notifications =
    abstract member add_temporary_notification : Notification_Data_2 -> unit
    abstract member get_permanent_notification_before_eval_js : unit -> string
    abstract member set_permanent_notification_before_eval_js : string -> unit
    abstract member set_permanent_notification_after_eval_js : string -> unit
    abstract member get_state : unit -> Notifications_Saveable_State
    abstract member set_state : Notifications_Saveable_State -> unit
(* We do not use this for now. *)
//    abstract member get_configuration : unit -> Temporary_Notifications_Configuration
    abstract member set_configuration : Temporary_Notifications_Configuration -> unit
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



(* Main functions - rendering *)

// TODO1 Add cross-fade for changes to permanent notification data. See if we can use Fade_Label from temporary notifications.
let view
    (is_visible : bool)
    (permanent_notification_data : string)
    (temporary_notification_component : IRefValue<ReactElement>)
    : ReactElement =

    if is_visible then
        Html.div [
            prop.id "notifications_container"
            prop.style [style.zIndex notifications_z_index]
            prop.children [
                if permanent_notification_data.Length > 0 then
                    Html.label [
                        prop.key permanent_notification_data
                        prop.className "notification"
                        prop.text permanent_notification_data
                    ]
                    Html.br []
                temporary_notification_component.current
            ]
        ]
    else Html.none

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

(* Component *)

[<ReactComponent>]
let Notifications (
    props : {| expose : IRefValue<I_Notifications> |},
    initial_configuration : Temporary_Notifications_Configuration
) : ReactElement =

(* State *)

(* We store the configuration in an IRefValue so the player can update it. *)
    let configuration_ref = React.useRef initial_configuration

    let is_visible, set_is_visible = React.useState true

    let queue : IRefValue<Temporary_Notifications_Queue> = React.useRef []

(* When we define JavaScript functions in start.txt, we might not be able to define all values or functions used by JavaScript expressions in the permanent notification text right away. As a result, trying to evaluate these expressions might fail at first. *)
    let permanent_notification_data_before_eval_js = React.useRef String.Empty
(* We use useState because we want this component to redraw itself whenever this value changes. *)
    let permanent_notification_data_after_eval_js, set_permanent_notification_data_after_js_eval = React.useState String.Empty

(* We need to pass a method from this interface (I_Temporary_Notification.show ()) to a callback (notify_remove_temporary_notification_from_queue) which we need to instantiate the component (Temporary_Notification_Component) that provides the interface. Therefore we use an IRefValue for the interface so we can "use" it before we have a working reference for it. *)
    let temporary_notification_interface : IRefValue<I_Temporary_Notification> = React.useRef <| Unchecked.defaultof<_>
    let temporary_notification_component = React.useRef <| Temporary_Notification_Component ({| expose = temporary_notification_interface |}, (fun () -> notify_remove_temporary_notification_from_queue queue temporary_notification_interface.current.show), initial_configuration)

(* Interface *)

    do React.useImperativeHandle (props.expose, fun () ->
        {
            new I_Notifications with
                member _.add_temporary_notification (data : Notification_Data_2) : unit = add_temporary_notification queue temporary_notification_interface.current.show data
                member _.get_permanent_notification_before_eval_js () : string = permanent_notification_data_before_eval_js.current
                member _.set_permanent_notification_before_eval_js (data : string) : unit = do permanent_notification_data_before_eval_js.current <- data
                member _.set_permanent_notification_after_eval_js (data : string) : unit = do set_permanent_notification_data_after_js_eval data
                member _.get_state () : Notifications_Saveable_State =
                    {
                        permanent_notification_before_eval_js = permanent_notification_data_before_eval_js.current
                        permanent_notification_after_eval_js = permanent_notification_data_after_eval_js
                    }
                member _.set_state (data : Notifications_Saveable_State) : unit =
                    do
                        permanent_notification_data_before_eval_js.current <- data.permanent_notification_before_eval_js
                        set_permanent_notification_data_after_js_eval data.permanent_notification_after_eval_js
(* We do not use this for now. *)
//                member _.get_configuration () : Temporary_Notifications_Configuration = configuration_ref.current
                member _.set_configuration (configuration : Temporary_Notifications_Configuration) : unit =
                    do
                        configuration_ref.current <- configuration
                        temporary_notification_interface.current.set_configuration configuration
                member _.is_visible () : bool = is_visible
                member _.show () : unit = set_is_visible true
                member _.hide () : unit = set_is_visible false
                member _.show_game_paused_notification () : unit = add_temporary_notification queue temporary_notification_interface.current.show { text = game_paused_notification }
        }
    )
(* This component does not implement I_Transitionable. *)

(* Render *)
    view is_visible permanent_notification_data_after_eval_js temporary_notification_component
