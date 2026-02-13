module Notification_Types

open JavaScript_Interop_1
open Units_Of_Measure

// TODO1 #notifications Notifications can obstruct image map hotspots. They should pass through mouse clicks.

(* TODO1 #notifications We need additional commands:
- hidestatus
    - hidestatus should also be plugged into hide UI command.
- clearstatus
*)

(* TODO2 #notifications Add the following animations:
x 1 When the permanent notification goes from Hidden to Visible or Visible to Hidden, or the temporary notification queue goes from empty to non-empty or from non-empty to empty, fade in or out the background blur.
/ 2 When the permanent notification cross-fades, or the temporary notification fades out and a new temporary notification fades in, animate the width value of the corresponding label or label container so the background blur expands or contracts. We tried cross fading the background blur but it doesn't work.

x To do this, we would probably need to expand Fade_Label to include the label container.

Example of style.width setting for label:
style.width (length.em (data.text.Length + 1))
(end)
*)

(* Types *)

type Notification_Type =
    | Temporary
    | Permanent

type Notification_Transition_Type = Fade

type Notifications_Configuration = {
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

type I_Temporary_Notification =
    abstract member show : Notification_Data_2 -> unit
    abstract member set_configuration : Notifications_Configuration -> unit

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

(* Consts *)

let max_temporary_notification_display_time = 30<seconds>
let min_temporary_notification_display_time = 1<seconds>
let max_notification_transition_time = 5<seconds>
let min_notification_transition_time = 0<seconds>

let notify_fade_in_complete = 0<command_queue_item_id>
let notify_fade_out_complete = 1<command_queue_item_id>
