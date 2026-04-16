module Notification_Types

open JavaScript_Interop_1
open Units_Of_Measure

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

// TODO2 #notifications Exporting saved game with save/load screen open stops temporary notifications from being shown. We currently cannot reproduce this.

type Pause_Notification_Type =
    | Game_Paused
    | Save_Complete
    | Quicksave_Complete
    | Load_Complete
    | Import_Complete
    | Export_Complete

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
    abstract member hide_permanent_notification : unit -> unit
    abstract member get_state : unit -> Notifications_Saveable_State
    abstract member set_state : Notifications_Saveable_State -> unit
(* We do not use this for now. *)
//    abstract member get_configuration : unit -> Temporary_Notifications_Configuration
    abstract member set_configuration : Notifications_Configuration -> unit
    abstract member show : unit -> unit
    abstract member hide : unit -> unit
    abstract member is_visible : unit -> bool
(* When we pause the game by calling Runner_Transition.force_complete_transitions (), we want to show a temporary notification to tell the player the game is paused and they must click to continue.
x After the player closes the save/load screen without loading or saving a game.
x After the player closes the configuration screen.
x After the player quicksaves.
x After the player saves a game, which closes the save/load screen.
x After the player loads a game, which closes the save/load screen.
x After the player imports the current game from a file, which closes the save/load screen if it is open.
x After the player exports the current game to a file, which closes the save/load screen if it is open. Okay, it looks like it does not close it and does not notify?
x Exceptions:
Runner_Save_Load.autosave_or_quicksave () for autosave. We autosave when we show a menu or image map, and after a jump.
Runner_Queue.run ()
Runner_History.undo_redo ()
*)
    abstract member show_pause_notification : Pause_Notification_Type -> unit

(* Consts *)

let max_temporary_notification_display_time = 30<seconds>
let min_temporary_notification_display_time = 1<seconds>
let max_notification_transition_time = 5<seconds>
let min_notification_transition_time = 0<seconds>

let notify_fade_in_complete = 0<command_queue_item_id>
let notify_fade_out_complete = 1<command_queue_item_id>

let game_paused_notification = "Game paused. Click to continue."
let save_complete_notification = "Save complete. Game paused. Click to continue."
let quicksave_complete_notification = "Quicksave complete. Game paused. Click to continue."
let load_complete_notification = "Load complete. Game paused. Click to continue."
let import_complete_notification = "Saved game import complete. Game paused. Click to continue."
let export_complete_notification = "Saved game export complete. Game paused. Click to continue."

(* Helper functions *)

let pause_notification_type_to_string = function
    | Game_Paused -> game_paused_notification
    | Save_Complete -> save_complete_notification
    | Quicksave_Complete -> quicksave_complete_notification
    | Load_Complete -> load_complete_notification
    | Import_Complete -> import_complete_notification
    | Export_Complete -> export_complete_notification
