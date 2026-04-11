module Save_Load_Validation

// DateTime
open System

// console, window
open Browser.Dom
// jsNative
open Fable.Core
// Decode
open Thoth.Json

open JavaScript_Interop_1
open Log
open Runner_Types_1
open Save_Load_Types
open Utilities

(* Consts *)

(* Validation bounds for imported save files. *)
let private max_import_file_length = 5_000_000
let private max_imported_saved_games = 100
let private max_saved_game_name_length = 40
let private max_screenshot_length = 2_000_000
let private max_runner_saveable_state_json_length = 2_000_000
let private max_javascript_state_json_length = 2_000_000
let private min_allowed_save_timestamp = DateTime(2000, 1, 1, 0, 0, 0, DateTimeKind.Utc)
let private max_allowed_save_time_from_now = TimeSpan.FromDays 1.0
let private max_menu_variables = 1_000
let private max_menu_variable_name_length = 128

(* Functions - helpers *)

let private is_valid_data_url_screenshot (screenshot : string) : bool =
    screenshot.StartsWith "data:image/" && screenshot.Contains ";base64,"

(* See also JavaScript_Interop_1.eval_js ()/eval_js_with_exception (). *)
[<Emit("JSON.parse($0)")>]
let private parse_json (json : string) : obj = jsNative

[<Emit("$0 !== null && typeof $0 === 'object' && !Array.isArray($0)")>]
let private is_plain_object (value : obj) : bool = jsNative

(* Functions - JavaScript state *)

(* This makes sure we can parse the JavaScript state JSON. However, it does not set the JavaScript state. That is done by JavaScript_Interop_2.set_javascript_state_with_exception (), which is called by Runner_State.set_state (), which is called by Runner_Save_Load.load_game () and Runner_History.undo_redo ().
*)
let validate_javascript_state
    (javascript_state : string)
    : Result<unit, string * Error_Data> =

    if String.IsNullOrWhiteSpace javascript_state then
        Error ("JavaScript state is empty.", [])

(* We already checked the Runner_Saveable_State length in validate_and_parse_runner_saveable_state (), and the JavaScript state is part of the Runner_Saveable_State. However, this function can also be called from JavaScript_Interop_2.set_javascript_state_with_exception (), which does not check the JavaScript state length, so we do it here. *)
    elif javascript_state.Length > max_javascript_state_json_length then
        Error ("JavaScript state is too large.", ["javascript_state_size_in_characters", javascript_state.Length; "max_javascript_state_size_in_characters", max_javascript_state_json_length])

    else
        try
            if javascript_state |> parse_json |> is_plain_object then Ok ()
            else
                Error ("JavaScript state must be an object and cannot be null or an array.", ["javascript_state", javascript_state])
        with e ->
            Error ("Failed to parse JavaScript state JSON.", ["message", e.Message])

(* Functions - Runner saveable state *)

let private validate_menu_variables
    (menu_variables : Menu_Variables)
    : Result<unit, string * Error_Data> =

    if menu_variables.Count > max_menu_variables then
        Error ("Save state contains too many menu variables.", ["number_of_menu_variables", menu_variables.Count; "maximum_number_of_menu_variables", max_menu_variables])
    else
        menu_variables |> Seq.map (fun (kv : Collections.Generic.KeyValuePair<string, int>) ->
            if String.IsNullOrWhiteSpace kv.Key then
                Error ("Menu variable name is empty.", [])
            elif kv.Key.Length > max_menu_variable_name_length then
                Error ("Menu variable name is too long.", ["menu_variable_name", kv.Key :> obj; "menu_variable_name_length", kv.Key.Length; "max_menu_variable_name_length", max_menu_variable_name_length])
            else
                Ok ()
        )
        |> Seq.tryFind (function | Error _ -> true | Ok _ -> false)
        |> function
            | Some e -> e
            | _ -> Ok ()

let private validate_runner_saveable_state
    (saved_state : Runner_Saveable_State)
    : Result<unit, string * Error_Data> =

    match saved_state with
    | Runner_Saveable_State_Running data ->
        match validate_javascript_state data.component_data.javascript_state_json with
        | Error e -> Error e
        | Ok () -> validate_menu_variables data.menu_variables
    | Runner_Saveable_State_Done data ->
        validate_javascript_state data.javascript_state_json

let validate_and_parse_runner_saveable_state
    (runner_saveable_state : string)
    : Result<Runner_Saveable_State, string * Error_Data> =

    if String.IsNullOrWhiteSpace runner_saveable_state then
        Error ("Saved game state cannot be empty.", [])
    elif runner_saveable_state.Length > max_runner_saveable_state_json_length then
        Error ("Saved game state is too large.", ["game_state_size_in_characters", runner_saveable_state.Length; "max_game_state_size_in_characters", max_runner_saveable_state_json_length])
    else
        match Decode.Auto.fromString<Runner_Saveable_State> runner_saveable_state with
        | Ok saved_state ->
            match validate_runner_saveable_state saved_state with
            | Error e -> Error e
            | Ok () -> Ok saved_state
        | Error message ->
            Error ("Failed to deserialize saved game state.", ["error_message", message])

(* Functions - files *)

let validate_saved_game_name
    (saved_game_name : string)
    : Result<unit, string * Error_Data> =

    if String.IsNullOrWhiteSpace saved_game_name then
        Error ("Saved game name is empty.", [])
    elif saved_game_name.Length > max_saved_game_name_length then
        Error ("Saved game name is too long.", ["name", saved_game_name; "name_length", saved_game_name.Length; "max_name_length", max_saved_game_name_length])
    elif not (is_valid_name saved_game_name) then
        Error ($"Saved game must contain only valid characters: {valid_name_characters}.", ["name", saved_game_name])
    else
        Ok ()

(* New_Saved_Game is what we store in storage and in exported files. It contains Runner_Saveable_State plus metadata (the ID, name, screenshot, and timestamp) that the player uses to select a game from the save/load screen. Runner_Saveable_State is what actually contains the game state and is what we use to load the game. We validate the metadata to make sure the player is not trying to import a corrupted or malicious exported file.
*)
let validate_saved_game
    (saved_game : New_Saved_Game)
    : Result<Runner_Saveable_State, string * Error_Data> =

    match validate_saved_game_name saved_game.name with

    | Error x -> Error x

    | Ok () ->
        let max_allowed_save_timestamp = DateTime.UtcNow.Add max_allowed_save_time_from_now

        if saved_game.screenshot.Length > max_screenshot_length then
            Error ("Screenshot is too large.", ["screenshot_size_in_characters", saved_game.screenshot.Length; "max_screenshot_size_in_characters", max_screenshot_length])
        elif not (is_valid_data_url_screenshot saved_game.screenshot) then
            Error ("Screenshot must be a data URL in image base64 format.", [])
        elif saved_game.timestamp < min_allowed_save_timestamp || saved_game.timestamp > max_allowed_save_timestamp then
            Error ("Saved game timestamp is outside the allowed range.", ["timestamp", saved_game.timestamp.ToString date_time_format; "min_allowed_save_timestamp", min_allowed_save_timestamp.ToString date_time_format; "max_allowed_save_timestamp", max_allowed_save_timestamp.ToString date_time_format; "max_allowed_save_time_from_now", max_allowed_save_time_from_now.ToString ()])
        elif String.IsNullOrWhiteSpace saved_game.runner_saveable_state_json then
            Error ("Saved game state is empty.", [])
        elif saved_game.runner_saveable_state_json.Length > max_runner_saveable_state_json_length then
            Error ("Saved game state is too large.", ["game_state_size_in_characters", saved_game.runner_saveable_state_json.Length; "max_game_state_size_in_characters", max_runner_saveable_state_json_length])
        else
            validate_and_parse_runner_saveable_state saved_game.runner_saveable_state_json

let validate_saved_games
    (saved_games : New_Saved_Game list)
    : Result<unit, string * Error_Data> =

    if List.isEmpty saved_games then
(* This is called by Save_Load_Storage.import_saved_game_or_games_from_file (), which adds the file name to the error info if needed. *)
        Error ("File does not contain any saved games.", [])
    elif saved_games.Length > max_imported_saved_games then
        Error ("File contains too many saved games.", ["number_of_saved_games", saved_games.Length; "max_number_of_saved_games", max_imported_saved_games])
    else
        saved_games
            |> List.mapi (fun index saved_game ->
(* validate_saved_game () makes sure we can deserialize the saved game state and Runner_Saveable_State so we do not import any saved game the player cannot load. *)
                match validate_saved_game saved_game with

                | Error (message, data) ->
                    Error ($"Could not validate saved game at index {index + 1}.", ["error_message", message :> obj] @ data)

                | Ok _ -> Ok ()

            )
            |> List.tryFind (function | Error _ -> true | Ok _ -> false)
            |> function
                | Some e -> e
                | _ -> Ok ()

(* This does not try to parse the file contents. The caller must do that. *)
let validate_import_file_contents
    (file_contents : string)
    : Result<New_Saved_Game list, string * Error_Data> =

    if String.IsNullOrWhiteSpace file_contents then
        Error ("File is empty.", [])
    elif file_contents.Length > max_import_file_length then
        Error ("File is too large.", ["file_size", file_contents.Length; "max_file_size", max_import_file_length])
    else
        match Decode.Auto.fromString<New_Saved_Game list> file_contents with

(* For simplicity, we always export a list of saved games, whether we are exporting all saved games or only the current game. *)
        | Ok saved_games ->
            if List.isEmpty saved_games then
                Error ("File does not contain any saved games.", [])
            else
                Ok saved_games

        | Error message ->
            Error ("Failed to deserialize saved games from file.", ["error_message", message])
