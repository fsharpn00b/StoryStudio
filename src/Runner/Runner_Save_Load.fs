module Runner_Save_Load

// String.Empty
open System

// window
open Browser.Dom
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
    (history : IRefValue<Runner_History>)
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (saved_game_state : string)
    : unit =

    match Decode.Auto.fromString<Runner_Saveable_State> saved_game_state with
    | Ok saved_state ->
        do
(* Clear the history. *)
            history.current <- { current_index = None; history = [] }
(* We do not need to call force_complete_transitions () because we did so before showing the saved game screen.
Each UI component's set_state () method dispatches Show or Hide messages with notify_transition_complete = false to prevent commands from auto-continuing unexpectedly.
*)
            set_state saved_state queue runner_components
(* If the state was saved at a point where it should be added to the history (typically, this means a point where we are waiting for player input), add the state to the history again. We need to do this after calling set_state (), because add_to_history () calls get_state ().
*)
(* See also Runner_Transition.notify_transition_complete (). *)
            match saved_state with
            | Runner_Saveable_State_Running data when data.add_to_history ->
(* Include a delay to make sure set_state finishes updating runner_components and command_state. *)
                window.setTimeout((fun () ->
                    add_to_history history runner_components queue
                ), int notify_transition_complete_delay_time) |> ignore
            | _ -> ()
    | _ -> error "load_game" "Failed to deserialize saved game." ["saved_game_state", saved_game_state] |> invalidOp

let get_load_game
    (runner_components : IRefValue<Runner_Components>)
    (history : IRefValue<Runner_History>)
    (queue : IRefValue<Runner_Queue>)
    : string -> unit =
(* Close load_game () over all these parameters so it becomes a string -> unit that we can pass to UI components.
We also need to delay the evaluation of this function until runner_components_1 is not None. The delayed result of this function is passed to the constructors of these components.
We can close over command_state because it is a reference.
*)
    fun (saved_game_state : string) -> load_game history queue runner_components saved_game_state

let quicksave_or_autosave
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (quicksave_or_autosave : Quicksave_Or_Autosave)
    : unit =

    do force_complete_transitions runner_components queue true (fun () ->
        let runner_state = get_state runner_components queue
        let json = Encode.Auto.toString (0, runner_state)
        do runner_components.current.save_load.current.quicksave_or_autosave json quicksave_or_autosave
    )

let export_current_game_to_file
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =

    do force_complete_transitions runner_components queue true (fun () ->
        let runner_state = get_state runner_components queue
        let json = Encode.Auto.toString (0, runner_state)
        do runner_components.current.save_load.current.export_current_game_to_file json
    )

let import_current_game_from_file
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    : unit =
    
    do force_complete_transitions runner_components queue true (fun () ->
        runner_components.current.save_load.current.import_current_game_from_file ()
    )

let show_saved_game_screen
    (queue : IRefValue<Runner_Queue>)
    (runner_components : IRefValue<Runner_Components>)
    (action : Saved_Game_Action)
    : unit =

(* Stop all transitions before we show the saved game screen. *)
    do force_complete_transitions runner_components queue true (fun () ->
        #if debug
        do debug "show_saved_game_screen" "Runner_State.force_complete_transitions () done." []
        #endif

        let runner_state = get_state runner_components queue
        let json = Encode.Auto.toString (0, runner_state)
        do runner_components.current.save_load.current.show action json
    )
