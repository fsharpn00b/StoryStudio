module Runner_Save_Load

// console, window
open Browser.Dom
// IRefValue
open Feliz
// Decode, Encode
open Thoth.Json

open Log
open Runner_State
open Runner_History
open Runner_Transition
open Runner_Types
open Save_Load_Types
open Utilities

(* Debug *)

let debug_module_name = "Runner_Save_Load"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions - save and load games *)

// TODO2 This transcends Runner? No, configuration should be stored separately anyway.
let private load_game
    (runner_state : Runner_State)
    (history : IRefValue<Runner_History>)
    (saved_game_state : string)
    : unit =

    match Decode.Auto.fromString<Runner_Saveable_State> saved_game_state with
    | Ok saved_state ->
        do
(* Clear the history. *)
            clear_history history
(* We do not need to call force_complete_transitions () because we did so before showing the saved game screen.
Each UI component's set_state () method dispatches Show or Hide messages with notify_transition_complete = false to prevent commands from auto-continuing unexpectedly.
*)
            set_state runner_state saved_state
(* If the state was saved at a point where it should be added to the history (typically, this means a point where we are waiting for player input), add the state to the history again. We need to do this after calling set_state (), because add_to_history () calls get_state ().
*)
(* See also Runner_Transition.notify_transition_complete (). *)
            match saved_state with
            | Runner_Saveable_State_Running data when data.add_to_history ->
(* Include a delay to make sure set_state finishes updating runner_component_interfaces and command_state. *)
                window.setTimeout((fun () ->
                    add_to_history runner_state history
                ), int notify_transition_complete_delay_time) |> ignore
            | _ -> ()
    | _ -> error "load_game" "Failed to deserialize saved game." ["saved_game_state", saved_game_state] |> invalidOp

let get_load_game
    (runner_state : Runner_State)
    (history : IRefValue<Runner_History>)
    : string -> unit =
(* Close load_game () over all these parameters so it becomes a string -> unit that we can pass to UI components.
We also need to delay the evaluation of this function until runner_component_interfaces_1 is not None. The delayed result of this function is passed to the constructors of these components.
We can close over command_state because it is a reference.
*)
(* TODO2 #pause For now, we do not show the pause notification when we actually load a game. That is because we already do it when we hide the save/load screen (which we do after we load a game) and when the player presses f to import a saved game from a file (whether they proceed to load the game or not).
*)
    fun (saved_game_state : string) -> do load_game runner_state history saved_game_state

let quicksave_or_autosave
    (runner_state : Runner_State)
    (quicksave_or_autosave : Quicksave_Or_Autosave)
    : unit =

    do
// TODO2 We should also show a warning in the save/load screen. Even surf supports indexeddb, so this is not high priority.
        if not is_indexeddb_supported then
            match quicksave_or_autosave with
            | Quicksave -> window.alert $"This browser does not support IndexedDB. {warn_recommendation}"
            | Autosave -> ()
        else
            force_complete_transitions runner_state true (fun () ->
                let runner_saveable_state = get_state runner_state
                let json = Encode.Auto.toString (0, runner_saveable_state)
                do runner_state.runner_component_interfaces.current.save_load.current.quicksave_or_autosave json quicksave_or_autosave
            )

let export_current_game_to_file
    (runner_state : Runner_State)
    : unit =

    do force_complete_transitions runner_state true (fun () ->
        let runner_saveable_state = get_state runner_state
        let json = Encode.Auto.toString (0, runner_saveable_state)
        do runner_state.runner_component_interfaces.current.save_load.current.export_current_game_to_file json
    )

let import_current_game_from_file
    (runner_state : Runner_State)
    : unit =
    
    do force_complete_transitions runner_state true (fun () ->
        runner_state.runner_component_interfaces.current.save_load.current.import_current_game_from_file ()
    )

let export_saved_games_from_storage_to_file
    (runner_state : Runner_State)
    : unit =
    
    do force_complete_transitions runner_state true (fun () ->
        runner_state.runner_component_interfaces.current.save_load.current.export_saved_games_from_storage_to_file ()
    )

let import_saved_games_from_file_to_storage
    (runner_state : Runner_State)
    : unit =
    
    do force_complete_transitions runner_state true (fun () ->
        runner_state.runner_component_interfaces.current.save_load.current.import_saved_games_from_file_to_storage ()
    )

let show_saved_game_screen
    (runner_state : Runner_State)
    (action : Saved_Game_Action)
    : unit =

(* Stop all transitions before we show the saved game screen. *)
    do force_complete_transitions runner_state true (fun () ->
        #if debug
        do debug "show_saved_game_screen" "Runner_State.force_complete_transitions () done." []
        #endif

        let runner_saveable_state = get_state runner_state
        let json = Encode.Auto.toString (0, runner_saveable_state)
        do runner_state.runner_component_interfaces.current.save_load.current.show action json
    )

(* TODO2 Issues with surf. We put this here because these issues mostly seem to involve key input, and many key inputs involve save/load.
- q, l and d are ignored.
- Esc shows configuration screen, but then does not dismiss it. Could be surf is overriding its behavior.
- x shows an error.
(end)

x We probably need a clickable menu to fix this. We'll need that anyway for mobile. Menu should contain
Config
Save
Load
Undo
Redo
(end)

Notes
- surf does support indexeddb.
- Run surf with -N to see dev console.
(end)
*)
