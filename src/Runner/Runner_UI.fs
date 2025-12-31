module Runner_UI

// String.Empty
open System

// Event, KeyboardEvent
open Browser.Types
// IRefValue
open Feliz

open Command_Types
open JavaScript_Interop
open JavaScript_Parser
open Log
open Runner_History
open Runner_Queue
open Runner_Save_Load
open Runner_Transition
open Runner_Types
open Save_Load_Types

(* TODO2 See if we can encode this into a single function.

20251226 This should now be partly handled by stopping click event propagation.

The player can do the following inputs.
1 Mouse click (calls run ()).
2 Mouse wheel scroll (calls undo ()/redo()).
3 Key press (calls handle_key_down ()).

We should allow/deny input as follows.
M = Menu
P = Image map
S = Save/load screen
C = Configuration screen

Action
S = Switch
H = Hide

        M   P   S   C
1       N   N   N   N
2       Y   Y   N   N
3
s       Y   Y   S   H
l       Same
d       Same
Esc     1   1   2   3
q       Y   Y   H   H
e       Y   Y   Y   Y
i       Y   Y   Y   Y
x       Y   Y   H   H
f       Y   Y   Y   Y
c       Y   Y   H   1
g       Y   Y   Y   Y
u       Y   Y   Y   Y

1 Show configuration screen.
2 Hide save/load screen and show configuration screen.
3 Hide configuration screen.
*)

(* Debug *)

let debug_module_name = "Runner_UI"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

let get_menu_variables
    (queue : IRefValue<Runner_Queue>)
    : Menu_Variables =

    match queue.current with
    | Queue_Idle data -> data.menu_variables
    | Queue_Loading data -> data.menu_variables
    | Queue_Running data
    | Queue_Interrupting data -> data.menu_variables
    | Queue_Done -> Map.empty

(* I_Runner implementation *)

(* These functions centralize the logic of showing/hiding UI elements as needed before proceeding. *)

let run
    (scenes : Scene_Map)
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (reason : Run_Reason)
    : unit =

(* When a menu or the save/load screen or configuration screen is visible, we do not want to run the next command. We could simply ignore mouse clicks except for those handled by the menu or save/load screen or configuration, but this is safer, in case this method is called for some other reason. *)
    if not <| runner_components.current.menu.current.is_visible () &&
        not <| runner_components.current.image_map.current.is_visible () &&
        not <| runner_components.current.save_load.current.is_visible () &&
        not <| runner_components.current.configuration.current.is_visible () then
(* We must determine what the next command is before we can run it. *)
        do run queue scenes runner_components reason

let show_or_hide_configuration_screen
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

    do
        if runner_components.current.configuration.current.is_visible () then
            runner_components.current.configuration.current.hide ()
        else
(* The save/load and configuration screens cannot be open at the same time. *)
            if runner_components.current.save_load.current.is_visible () then
                do runner_components.current.save_load.current.hide ()
            force_complete_transitions runner_components queue true (fun () ->
                do runner_components.current.configuration.current.show ()
            )

// We do not use this for now.
(*
let hide_configuration_screen
    (runner_components : IRefValue<Runner_Components>)
    : unit =
    if runner_components.current.configuration.current.is_visible () then
        do runner_components.current.configuration.current.hide ()
*)

let handle_escape_key
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

    do
        if runner_components.current.configuration.current.is_visible () then
            runner_components.current.configuration.current.hide ()
        elif runner_components.current.save_load.current.is_visible () then
            runner_components.current.save_load.current.hide ()
        else
            force_complete_transitions runner_components queue true (fun () ->
                do runner_components.current.configuration.current.show ()
            )

let show_saved_game_screen
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (action : Saved_Game_Action)
    : unit =

    do
        if runner_components.current.save_load.current.is_visible () then
            runner_components.current.save_load.current.switch action
        else
(* The save/load and configuration screens cannot be open at the same time. *)
            if runner_components.current.configuration.current.is_visible () then
                runner_components.current.configuration.current.hide ()
            show_saved_game_screen queue runner_components action

// We do not use this for now.
(*
let hide_saved_game_screen
    (runner_components : IRefValue<Runner_Components>)
    : unit =
    if runner_components.current.save_load.current.is_visible () then
        do runner_components.current.save_load.current.hide ()
*)

let show_or_hide_ui
    (runner_components : IRefValue<Runner_Components>)
    : unit =

    do
(* We do not request notification when the transition completes, or provide a command queue item ID, because this is an internal command. *)
        if runner_components.current.dialogue_box.current.is_visible () ||
            runner_components.current.command_menu.current.is_visible () ||
            runner_components.current.notifications.current.is_visible () then
            runner_components.current.dialogue_box.current.hide false None
            runner_components.current.command_menu.current.hide ()
            runner_components.current.notifications.current.hide ()
        else
            runner_components.current.dialogue_box.current.show false None
            runner_components.current.command_menu.current.show ()
            runner_components.current.notifications.current.show ()

let quicksave
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

(* If the save/load or configuration screen is visible, hide it so we can get an accurate screenshot of the player's game. *)
    do
        if runner_components.current.save_load.current.is_visible () then
            runner_components.current.save_load.current.hide ()
        elif runner_components.current.configuration.current.is_visible () then
            runner_components.current.configuration.current.hide ()
        quicksave_or_autosave queue runner_components Quicksave
        runner_components.current.notifications.current.show_game_paused_notification ()

let export_current_game_to_file
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

(* If the save/load or configuration screen is visible, hide it so we can get an accurate screenshot of the player's game. *)
    do
        if runner_components.current.save_load.current.is_visible () then
            runner_components.current.save_load.current.hide ()
        elif runner_components.current.configuration.current.is_visible () then
            runner_components.current.configuration.current.hide ()
        export_current_game_to_file queue runner_components
(* TODO2 #pause There is no way to detect when the player closes the save file dialogue, and therefore no way to delay this notification. We could have a temporary notification that is dismissed by a click instead of using a timer. That should be separate from timed notifications, so it doesn't hide them. *)
        runner_components.current.notifications.current.show_game_paused_notification ()

let import_current_game_from_file
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

    do
(* See comments for export_current_game_to_file (). We have the same issue with the open file dialogue. *)
        Runner_Save_Load.import_current_game_from_file queue runner_components
        runner_components.current.notifications.current.show_game_paused_notification ()

let export_saved_games_from_storage_to_file
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

(* See comments for export_current_game_to_file (). *)
    do
        Runner_Save_Load.export_saved_games_from_storage_to_file queue runner_components
        runner_components.current.notifications.current.show_game_paused_notification ()

let import_saved_games_from_file_to_storage
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

(* See comments for export_current_game_to_file (). *)
    do
        Runner_Save_Load.import_saved_games_from_file_to_storage queue runner_components
        runner_components.current.notifications.current.show_game_paused_notification ()

let undo
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (history : IRefValue<Runner_History>)
    : unit =

(* When the save/load or configuration screen is visible, we do not want to undo or redo. *)
    if not <| runner_components.current.save_load.current.is_visible () &&
        not <| runner_components.current.configuration.current.is_visible () then
        do undo_redo history queue runner_components true

let redo
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (history : IRefValue<Runner_History>)
    : unit =

(* When the save/load or configuration screen is visible, we do not want to undo or redo. *)
    if not <| runner_components.current.save_load.current.is_visible () &&
        not <| runner_components.current.configuration.current.is_visible () then
        do undo_redo history queue runner_components false

(* Event handlers *)

let handle_key_down
    (scenes : Scene_Map)
    (runner : IRefValue<I_Runner>)
    (event : Event)
    : unit =

    let event_2 = event :?> KeyboardEvent
    match event_2.key with
// TODO1 Replace these with literals or configuration values.
    | "s" -> runner.current.show_saved_game_screen Save_Game
    | "l" -> runner.current.show_saved_game_screen Load_Game
    | "d" -> runner.current.show_saved_game_screen Delete_Game
    | "Escape" -> runner.current.handle_escape_key ()
    | "q" -> runner.current.quicksave ()
    | "e" -> runner.current.export_saved_games_from_storage_to_file ()
    | "i" -> runner.current.import_saved_games_from_file_to_storage ()
    | Save_Load_Types.export_current_game_key -> runner.current.export_current_game_to_file ()
    | "f" ->
        do
            runner.current.import_current_game_from_file ()
(* For some reason, the "f" key has the same effect as clicking. *)
            event.stopPropagation ()
    | "c" -> runner.current.show_or_hide_configuration_screen ()
    | "g" -> runner.current.download_screenshot ()
    | "u" -> runner.current.show_or_hide_ui ()
    | _ -> ()
(* These are for debugging.
TODO1 Also add key handlers for
- h help
(end)
*)
    match event_2.key with
    | "Q" -> runner.current.show_queue ()
    | "C" -> runner.current.show_characters ()
    | "B" -> runner.current.show_background ()
(* After dumping the JavaScript to the browser console, copy it to a .ts file and run
npx tsc --noEmit --strict <file>
(end)
To install TypeScript, run
npm install typescript --save-dev
(end)
The --save-dev means you are installing the package only for dev purposes, not to be deployed with the application.
*)
    | "J" -> scenes |> check_javascript
    | "T" ->
        show_js_state ()
        runner.current.show_menu_variables ()
(* TODO1 #dynamic_load Temporary. This is a demonstration of how to import a React component dynamically. *)
    | "X" -> runner.current.show_plugin ()
    | _ -> ()
