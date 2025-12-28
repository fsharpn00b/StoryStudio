module Notifications

// Environment.NewLine
open System
// Dictionary
open System.Collections.Generic

// console
open Browser.Dom
open Elmish
open Feliz
open Feliz.UseElmish

open Fade_Transition
open Fade_Types
open Fade_Visibility
open Log
open Temporary_Notification
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Notifications"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

(* Types *)

type Temporary_Notifications_Queue = Temporary_Notification_Data list

// TODO1 Not implemented yet.
type Permanent_Notification = string
type Permanent_Notification_Saveable_State = string

(* Interfaces *)

type I_Notifications =
    abstract member add_temporary_notification : Temporary_Notification_Data -> unit
(* We do not use this for now. *)
//    abstract member get_configuration : unit -> Temporary_Notifications_Configuration
    abstract member set_configuration : Temporary_Notifications_Configuration -> unit

(* Global values *)

let mutable temporary_notification_queue_lock = 0

(* Main functions - state *)

// TODO1 This is for getting permanent notification state. Not implemented yet.
let private get_state (notifications : IRefValue<Permanent_Notification>) : Permanent_Notification_Saveable_State =
    String.Empty

(* Main functions - rendering *)

let view (notification : IRefValue<ReactElement>) : ReactElement =
    Html.div [
        prop.id "notifications_container"
        prop.style [style.zIndex notifications_z_index]
        prop.children [notification.current]
    ]

(* Main functions - state *)

let notify_remove_temporary_notification_from_queue
    (queue : IRefValue<Temporary_Notifications_Queue>)
    (show : Temporary_Notification_Data -> unit)
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
    (show : Temporary_Notification_Data -> unit)
    (data : Temporary_Notification_Data)
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

    let queue : IRefValue<Temporary_Notifications_Queue> = React.useRef []

(* We need to pass a method from this interface (I_Temporary_Notification.show ()) to a callback (notify_remove_temporary_notification_from_queue) which we need to instantiate the component (Temporary_Notification_Component) that provides the interface. Therefore we use an IRefValue for the interface so we can "use" it before we have a working reference for it. *)
    let temporary_notification_interface : IRefValue<I_Temporary_Notification> = React.useRef <| Unchecked.defaultof<_>
    let temporary_notification_component = React.useRef <| Temporary_Notification_Component ({| expose = temporary_notification_interface |}, (fun () -> notify_remove_temporary_notification_from_queue queue temporary_notification_interface.current.show), initial_configuration)

(* Interface *)

    do React.useImperativeHandle (props.expose, fun () ->
        {
            new I_Notifications with
                member _.add_temporary_notification (data : Temporary_Notification_Data) : unit = add_temporary_notification queue temporary_notification_interface.current.show data
(* We do not use this for now. *)
//                member _.get_configuration () : Temporary_Notifications_Configuration = configuration_ref.current
                member _.set_configuration (configuration : Temporary_Notifications_Configuration) : unit =
                    do
                        configuration_ref.current <- configuration
                        temporary_notification_interface.current.set_configuration configuration
        }
    )
(* This component does not implement I_Transitionable. *)

(* Render *)
    view temporary_notification_component

(* TODO1 When we pause, show temporary notification telling player to click to continue.
- After showing save/load screen
- After showing configuration screen
- After player presses key to import/export single/multiple games to/from file

- Basically any scenario where we force transition completion except Runner.run ().
*)

(* TODO1 How to implement.

- Permanent notifications
- Have author configure a json file for permanent notifications.
- Show them in order defined0 Or just have a single string that can include newlines.
- Create a set_(name)/get_(name) method in the javascript for each one?
- They can use JS interpolations, and we'll re-get those each time we re-display the notification?

- Any way we can auto-update? We would need to maybe expose a JS method/struct that mimics a JS var for the author, and when they "set" it, they are really pinging us. In that case they could specify it in a json file rather than use the notify command.

(end)

/ Temporary notifications
x Author issues a notify command.
x Show one notification at a time. If more than one, queue them up.
x Let player configure display time and fade in/out time.
- Save timeout handle to cancel notification in case we - what? load new game? undo?
- Also need to clear queue in that case?
(end)

- Both
- We need hide/show/is_visible for hide UI command. Use a fade state for that so we can just dispatch show/hide? We'll need to implement our own is_visible ().
- For now, when we get state (to save, add to history, etc.), we only worry about permanent notifications.

(end)

*)
