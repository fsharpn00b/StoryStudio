module Save_Load_File

// DateTime
open System

// console, window
open Browser.Dom
// Decode, Encode
open Thoth.Json

open Log
open Runner_Types_1
open Save_Load_Helpers
open Save_Load_Storage_Add
open Save_Load_Storage_Load
open Save_Load_Types
open Save_Load_Validation
open Utilities

(* Debug *)

let debug_module_name = "Save_Load_File"

let private debug : log_function = debug debug_module_name
let private warn
    (function_name : string)
    (alert : bool)
    (message : string)
    (data : (string * obj) list)
    : unit =
    warn debug_module_name function_name alert $"{message} {warn_recommendation}" data
let private error : error_function = error debug_module_name

(* Functions - export saved games to file *)

let export_saved_games_from_storage_to_file () : unit =
    let file_name = $"{get_current_timestamp ()}.json"

    promise {
        try
            let! result = get_all_saved_games_from_storage ()
            match result with

            | Ok saved_games_1 ->

(* Disard the database record IDs before exporting. If we re-import these saved games, we will assign new database record IDs then. *)
                let saved_games_2 = saved_games_1 |> List.map (fun saved_game ->
                    {
                        name = saved_game.name
                        screenshot = saved_game.screenshot
                        timestamp = saved_game.timestamp
                        runner_saveable_state_json = saved_game.runner_saveable_state_json
                    }
                )
                let json = Encode.Auto.toString (0, saved_games_2)
                download_file file_name "text/json" json

            | Error (message, error_data) ->

                warn "export_saved_games_from_storage_to_file" true message (["file_name", file_name] @ error_data)

        with e ->
            warn "export_saved_games_from_storage_to_file" true "Unknown error." ["file_name", file_name; "error", e]
    } |> Promise.iter ignore

let export_current_game_to_file (runner_saveable_state : string) : unit =
    match window.prompt ("Enter save name:", get_current_timestamp ()) with
    | null -> ()
    | save_name ->
        promise {
            let! canvas = get_canvas true
            return canvas
        } |> Promise.iter (fun canvas ->
            let file_name = $"{get_current_timestamp ()}.json"
            let saved_game = {
                name = save_name
                screenshot = downscale_screenshot canvas screenshot_max_width screenshot_mime_type screenshot_encoder_options
                timestamp = DateTime.UtcNow
                runner_saveable_state_json = runner_saveable_state
            }
(* import_saved_games_from_file () expects a list of saved games. *)
            let json = Encode.Auto.toString (0, [ saved_game ])
            download_file file_name "text/json" json
        )

(* Functions - import saved games from file *)

let private import_current_game
(* This is for error reporting. *)
    (file_name : string)
    (saved_games_1 : New_Saved_Game list)
(* load_game is Runner_State.load_game (), closed over runner_component_interfaces, history, and queue. All it needs is the saved game state. *)
    (load_game : Runner_Saveable_State -> unit)
    (dispatch : Save_Load_Message -> unit)
    : unit =

(* If the player imports a file with multiple saved games, we load the saved game with the latest timestamp. *)
    let saved_games_2 = saved_games_1 |> List.sortByDescending (fun saved_game -> saved_game.timestamp)
(* We have already made sure saved_games_2 is not empty in import_saved_game_or_games_from_file (). *)
    let saved_game = saved_games_2.Head
    match validate_saved_game saved_game with

    | Ok runner_saveable_state ->
        if window.confirm $"Load save '{saved_game.name}'? Current progress will be lost." then
            do load_game runner_saveable_state
(* Delay before we hide the save/load game screen, so the mouse click to select the saved game does not also cause us to call Runner_Queue.run (). For now, return the state unchanged. *)
            do window.setTimeout ((fun () ->
                dispatch <| Hide
            ), int hide_save_load_screen_delay_time) |> ignore

    | Error validation_error ->
        warn "import_current_game" true "Failed to validate saved game data." ["file_name", file_name; "error_message", validation_error]

(* TODO1 #save When we export or import all save games, do we include autosave and quicksave?
We use add to import games, which gives each a new ID, so we would duplicate autosave and quicksave.
We should probably, when importing all save games, look for the quicksave and autosave IDs, and use put for those.
*)
let private import_all_games
(* This is for error reporting. *)
    (file_name : string)
    (saved_games : New_Saved_Game list)
    (dispatch : Save_Load_Message -> unit)
    : unit =

(* We do this validation here, instead of in validate_import_file_contents (), because there is no need to validate all games if we only import one. *)
    match validate_saved_games saved_games with

    | Ok () ->
    
        promise {
            try
                let! result = add_saved_games_to_storage saved_games
                match result with

                | Ok () ->
                    do dispatch <| Redraw

                | Error (message, data) ->
                    warn "import_all_games" true message (["file_name", file_name] @ data)

            with e ->
                warn "import_all_games" true "Unknown error." ["file_name", file_name; "error", e]

        } |> Promise.iter ignore

    | Error (message, data) ->
        warn "import_all_games" true "Failed to validate saved game data." (["file_name", file_name; "error_message", message] @ data)

let import_saved_games_from_file
    (dispatch : Save_Load_Message -> unit)
    (file_name : string)
    (file_contents : string)
    : unit =

    match validate_import_file_contents file_contents with
    | Error (message, data) ->
        warn "import_saved_games_from_file" true "Failed to validate saved game file." (["file_name", file_name; "error_message", message] @ data)

    | Ok saved_games ->
        do import_all_games file_name saved_games dispatch

let import_saved_game_from_file
(* load_game is Runner_State.load_game (), closed over runner_component_interfaces, history, and queue. All it needs is the saved game state. *)
    (load_game : Runner_Saveable_State -> unit)
    (dispatch : Save_Load_Message -> unit)
    (file_name : string)
    (file_contents : string)
    : unit =

    match validate_import_file_contents file_contents with
    | Error (message, data) ->
        warn "import_saved_game_from_file" true "Failed to validate saved game file." (["file_name", file_name; "error_message", message] @ data)

    | Ok saved_games ->
        do import_current_game file_name saved_games load_game dispatch
