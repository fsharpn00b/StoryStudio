module Runner_UI

// console, window
open Browser.Dom
// Event, KeyboardEvent
open Browser.Types
// IRefValue
open Feliz

open Command_Types
open JavaScript_Interop_1
open JavaScript_Interop_2
open JavaScript_Parser
open Key_Bindings
open Log
open Runner_History
open Runner_Save_Load
open Runner_Transition
open Runner_Types
open Save_Load_Types
open Utilities

(* TODO2 See if we can encode this into a single function.

20251226 This should now be partly handled by stopping click event propagation.

The player can do the following inputs.
1 Mouse click (calls run ()).
2 Mouse wheel scroll (calls undo ()/redo()).
3 Command menu
4 Key press (calls handle_key_down ()).

We should allow/deny input as follows.
M = Menu
I = Image map
S = Save/load screen
C = Configuration screen

Action
S = Switch between save/load/delete modes.
H = Hide
N = Ignore

        M   I   S   C
1       N   N   N   N
2       Y   Y   N   N

3
save    Y   Y   S   Y
load    Y   Y   S   Y
undo    Y   Y   N   N
redo    Y   Y   N   N
config  Y   Y   Y   H

4
<space> N   N   N   N
s       Y   Y   S   N
l       Same
d       Same
Esc     1   1   H   H
q       Y   Y   H   N
e       Y   Y   Y   N
i       Y   Y   Y   N
x       Y   Y   H   N
f       Y   Y   H   N
c       Y   Y   2   N
g       Y   Y   Y   N
u       Y   Y   N   N

1 Show configuration screen.
2 Hide save/load screen and show configuration screen.
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
    (queue : IRefValue<Runner_Queue>)
    (scenes : Scene_Map)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (reason : Run_Reason)
    : unit =

(* We do not want to run the next command when any of the following are visible.
- Menu
- Image map
- Save/load screen
- Configuration screen
(end)
*)
    let continuation () : unit =
        if not <| runner_component_interfaces.current.menu.current.is_visible () &&
            not <| runner_component_interfaces.current.image_map.current.is_visible () &&
            not <| runner_component_interfaces.current.save_load.current.is_visible () &&
            not <| runner_component_interfaces.current.configuration.current.is_visible () then
(* We must determine what the next command is before we can run it. *)
            do Runner_Queue.run queue scenes runner_component_interfaces reason

// TODO2 #plugins Consider expanding this check to all runner components. So far we have only had to check plugins because they are loaded asynchronously.
    let are_plugins_ready () : bool =
        runner_component_interfaces.current.plugins |> Seq.exists (fun kv -> isNull kv.Value.interface_ref.current) |> not

    let rec poll_for_plugins_to_be_ready () : unit =
        do window.setTimeout ((fun () ->
            if are_plugins_ready () then do continuation ()
            else poll_for_plugins_to_be_ready ()
        ), int wait_for_plugin_interfaces_to_be_ready_time) |> ignore

(* window.state must be defined, so we provide an empty definition here in case the author does not define it. If they do, their definition will replace ours. *)
    if Initial_Run = reason then
        do
            set_state_in_js "{}"
            poll_for_plugins_to_be_ready ()
    else do continuation ()

let show_or_hide_configuration_screen
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : unit =

    do
        if runner_component_interfaces.current.configuration.current.is_visible () then
            runner_component_interfaces.current.configuration.current.hide ()
        else
(* The save/load and configuration screens cannot be open at the same time. *)
            if runner_component_interfaces.current.save_load.current.is_visible () then
                do runner_component_interfaces.current.save_load.current.hide ()
            force_complete_transitions runner_component_interfaces queue true (fun () ->
                do runner_component_interfaces.current.configuration.current.show ()
            )

let handle_escape_key
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : unit =

    do
        if runner_component_interfaces.current.configuration.current.is_visible () then
            runner_component_interfaces.current.configuration.current.hide ()
        elif runner_component_interfaces.current.save_load.current.is_visible () then
            runner_component_interfaces.current.save_load.current.hide ()
        else
            force_complete_transitions runner_component_interfaces queue true (fun () ->
                do runner_component_interfaces.current.configuration.current.show ()
            )

let show_saved_game_screen
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (action : Saved_Game_Action)
    : unit =

    do
        if runner_component_interfaces.current.save_load.current.is_visible () then
            runner_component_interfaces.current.save_load.current.switch action
        else
(* The save/load and configuration screens cannot be open at the same time. The player cannot switch from the configuration screen to the save/load game screen using the keyboard, but they can do so using the command menu. *)
            if runner_component_interfaces.current.configuration.current.is_visible () then
                runner_component_interfaces.current.configuration.current.hide ()
            show_saved_game_screen queue runner_component_interfaces action

let show_or_hide_ui
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : unit =

(* If the save/load screen is visible, ignore this. *)
    if not <| runner_component_interfaces.current.save_load.current.is_visible () then
        do
(* We do not request notification when the transition completes, or provide a command queue item ID, because this is an internal command. *)
            if runner_component_interfaces.current.dialogue_box.current.is_visible () ||
                runner_component_interfaces.current.command_menu.current.is_visible () ||
                runner_component_interfaces.current.notifications.current.is_visible () then
                runner_component_interfaces.current.dialogue_box.current.hide false None
                runner_component_interfaces.current.command_menu.current.hide ()
                runner_component_interfaces.current.notifications.current.hide ()
            else
                runner_component_interfaces.current.dialogue_box.current.show false None
                runner_component_interfaces.current.command_menu.current.show ()
                runner_component_interfaces.current.notifications.current.show ()

let quicksave
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : unit =

(* If the save/load screen is visible, hide it so we can get an accurate screenshot of the player's game. *)
    do
        if runner_component_interfaces.current.save_load.current.is_visible () then
            runner_component_interfaces.current.save_load.current.hide ()
        quicksave_or_autosave queue runner_component_interfaces Quicksave
        runner_component_interfaces.current.notifications.current.show_game_paused_notification ()

let export_current_game_to_file
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : unit =

(* If the save/load screen is visible, hide it so we can get an accurate screenshot of the player's game. *)
    do
        if runner_component_interfaces.current.save_load.current.is_visible () then
            runner_component_interfaces.current.save_load.current.hide ()
        Runner_Save_Load.export_current_game_to_file queue runner_component_interfaces
(* TODO2 #pause There is no way to detect when the player closes the save file dialogue, and therefore no way to delay this notification. We could have a temporary notification that is dismissed by a click instead of using a timer. That should be separate from timed notifications, so it doesn't hide them. *)
        runner_component_interfaces.current.notifications.current.show_game_paused_notification ()

let import_current_game_from_file
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : unit =

(* If the save/load screen is visible, hide it. *)
    do
        if runner_component_interfaces.current.save_load.current.is_visible () then
            runner_component_interfaces.current.save_load.current.hide ()
(* See comments for export_current_game_to_file (). We have the same issue with the open file dialogue. *)
        Runner_Save_Load.import_current_game_from_file queue runner_component_interfaces
        runner_component_interfaces.current.notifications.current.show_game_paused_notification ()

(* We do not hide the save/load screen for this, as it does not affect the current game. *)
let export_saved_games_from_storage_to_file
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : unit =

(* See comments for export_current_game_to_file (). *)
    do
        Runner_Save_Load.export_saved_games_from_storage_to_file queue runner_component_interfaces
        runner_component_interfaces.current.notifications.current.show_game_paused_notification ()

(* We do not hide the save/load screen for this, as it does not affect the current game. *)
let import_saved_games_from_file_to_storage
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : unit =

(* See comments for export_current_game_to_file (). *)
    do
        Runner_Save_Load.import_saved_games_from_file_to_storage queue runner_component_interfaces
        runner_component_interfaces.current.notifications.current.show_game_paused_notification ()

let undo
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (history : IRefValue<Runner_History>)
    : unit =

(* When the save/load or configuration screen is visible, we do not want to undo or redo. The save/load and configuration screens also prevent mouse wheel scroll events from propagating, and disable the undo/redo buttons in the command menu, but we are leaving this check here to be safe.
*)
    if not <| runner_component_interfaces.current.save_load.current.is_visible () &&
        not <| runner_component_interfaces.current.configuration.current.is_visible () then
        do undo_redo history queue runner_component_interfaces true

let redo
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (history : IRefValue<Runner_History>)
    : unit =

(* When the save/load or configuration screen is visible, we do not want to undo or redo. *)
    if not <| runner_component_interfaces.current.save_load.current.is_visible () &&
        not <| runner_component_interfaces.current.configuration.current.is_visible () then
        do undo_redo history queue runner_component_interfaces false

(* Event handlers *)

let handle_key_down
    (scenes : Scene_Map)
    (runner : IRefValue<I_Runner>)
    (event : Event)
    : unit =

    let key_bindings =
        (runner.current.get_key_bindings (), permanent_key_to_key_binding_names)
            ||> Seq.fold (fun acc item -> acc.Add (item.Key, item.Value))

    let event_2 = event :?> KeyboardEvent
    match key_bindings.TryFind event_2.key with
    | None -> ()
    | Some name ->
        match name with
        | "continue" -> runner.current.run Run_Reason.Player_Run
        | "undo" -> runner.current.undo ()
        | "redo" -> runner.current.redo ()
        | "save_game" -> runner.current.show_saved_game_screen Save_Game
        | "load_game" -> runner.current.show_saved_game_screen Load_Game
        | "delete_game" -> runner.current.show_saved_game_screen Delete_Game
        | "escape" -> runner.current.handle_escape_key ()
        | "quicksave" -> runner.current.quicksave ()
        | "export_saved_games" -> runner.current.export_saved_games_from_storage_to_file ()
        | "import_saved_games" -> runner.current.import_saved_games_from_file_to_storage ()
        | "export_current_game" -> runner.current.export_current_game_to_file ()
        | "import_current_game" ->
            do
                runner.current.import_current_game_from_file ()
(* For some reason, the "f" key has the same effect as clicking. *)
                event.stopPropagation ()
        | "configuration" -> runner.current.show_or_hide_configuration_screen ()
        | "get_screenshot" -> runner.current.download_screenshot ()
        | "ui" -> runner.current.show_or_hide_ui ()

(* These are for debugging. *)
        | "queue" -> runner.current.show_queue ()
        | "characters" -> runner.current.show_characters ()
        | "background" -> runner.current.show_background ()
(* After dumping the JavaScript to the browser console, copy it to a .ts file and run
npx tsc --noEmit --strict <file>
(end)
To install TypeScript, run
npm install typescript --save-dev
(end)
The --save-dev means you are installing the package only for dev purposes, not to be deployed with the application.
*)
        | "javascript" -> scenes |> check_javascript
        | "state" ->
            do
                show_js_state ()
                runner.current.show_menu_variables ()
        | _ -> warn "handle_key_down" true "Unrecognized key binding." ["key", event_2.key; "binding", name]
