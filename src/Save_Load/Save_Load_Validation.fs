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

(* Debug *)

let debug_module_name = "Save_Load_Validation"

let private debug : log_function = debug debug_module_name
let private warn
    (function_name : string)
    (alert : bool)
    (message : string)
    (data : Error_Data)
    : unit =
    warn debug_module_name function_name alert $"{message} {warn_recommendation}" data
let private error : error_function = error debug_module_name

(* Consts *)

// TODO1 Find out why these are security risks.
(* Validation bounds for imported save files. *)
let private max_import_file_length = 5_000_000
let private max_imported_saved_games = 100
let private max_saved_game_name_length = 40
let private max_screenshot_length = 2_000_000
let private max_saved_game_state_length = 2_000_000
let private min_allowed_save_timestamp = DateTime(2000, 1, 1, 0, 0, 0, DateTimeKind.Utc)
let private max_allowed_save_timestamp = DateTime.UtcNow.AddDays 1.0
let private max_menu_variables = 1_000
let private max_menu_variable_name_length = 128

(* Functions - save/load storage helpers *)

let private is_valid_data_url_screenshot (screenshot : string) : bool =
    screenshot.StartsWith "data:image/" && screenshot.Contains ";base64,"

(* See also JavaScript_Interop_1.eval_js ()/eval_js_with_exception (). *)
[<Emit("JSON.parse($0)")>]
let private parse_json (state_json : string) : obj = jsNative

[<Emit("$0 !== null && typeof $0 === 'object' && !Array.isArray($0)")>]
let private is_plain_object (value : obj) : bool = jsNative

(* Functions - save/load storage *)

let validate_import_file_contents
    (file_contents : string)
    : Result<unit, string * Error_Data> =

    if String.IsNullOrWhiteSpace file_contents then
        Error ("File is empty.", [])
    elif file_contents.Length > max_import_file_length then
        Error ("File is too large.", ["file_size", file_contents.Length; "max_file_size", max_import_file_length])
    else
        Ok ()

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

let validate_saved_game
    (saved_game : New_Saved_Game)
    : Result<unit, string * Error_Data> =

    let validate_saved_game_name_result = validate_saved_game_name saved_game.name
    match validate_saved_game_name_result with
    | Error x -> Error x
    | Ok () ->
        if saved_game.screenshot.Length > max_screenshot_length then
            Error ("Screenshot is too large.", ["screenshot_size_in_characters", saved_game.screenshot.Length; "max_screenshot_size_in_characters", max_screenshot_length])
        elif not (is_valid_data_url_screenshot saved_game.screenshot) then
            Error ("Screenshot must be a data URL in image base64 format.", [])
        elif saved_game.timestamp < min_allowed_save_timestamp || saved_game.timestamp > max_allowed_save_timestamp then
            Error ("Saved game timestamp is outside the allowed range.", ["timestamp", saved_game.timestamp.ToString date_time_format; "min_allowed_save_timestamp", min_allowed_save_timestamp.ToString date_time_format; "max_allowed_save_timestamp", max_allowed_save_timestamp.ToString date_time_format])
        elif String.IsNullOrWhiteSpace saved_game.game_state then
            Error ("Saved game state is empty.", [])
        elif saved_game.game_state.Length > max_saved_game_state_length then
            Error ("Saved game state is too large.", ["game_state_size_in_characters", saved_game.game_state.Length; "max_game_state_size_in_characters", max_saved_game_state_length])
        else
            Ok ()

let validate_saved_games_for_import
    (saved_games : New_Saved_Game list)
    : Result<unit, string * Error_Data> =

    if List.isEmpty saved_games then
        Error ("File does not contain any saved games.", [])
    elif saved_games.Length > max_imported_saved_games then
        Error ("File contains too many saved games.", ["number_of_saved_games", saved_games.Length; "max_number_of_saved_games", max_imported_saved_games])
    else
        saved_games
            |> List.mapi (fun index saved_game ->
                validate_saved_game saved_game
                    |> Result.mapError (fun (message, data) -> $"Saved game #{index + 1}: {message}", data)
            )
            |> List.tryFind (function | Error _ -> true | Ok _ -> false)
            |> function
                | Some (Error x) -> Error x
                | _ -> Ok ()

let validate_javascript_state_json
    (state_json : string)
    : unit =

// TODO1 #validation Make sure we can return a value from a try block.
    let parsed =
        try
            parse_json state_json
        with e ->
            error "validate_javascript_state_json" "Failed to parse JavaScript state JSON." ["message", e.Message] |> invalidOp

    if not <| is_plain_object parsed then
        error "validate_javascript_state_json" "JavaScript state must be a JSON object." ["state_json", state_json] |> invalidOp

(* Functions - Runner *)

let private validate_menu_variables
    (menu_variables : Menu_Variables)
    : unit =

    if menu_variables.Count > max_menu_variables then
        error "validate_menu_variables" "Save state contains too many menu variables." ["number_of_menu_variables", menu_variables.Count; "maximum_number_of_menu_variables", max_menu_variables] |> invalidOp

    menu_variables |> Seq.iter (fun (kv : Collections.Generic.KeyValuePair<string, int>) ->
        if String.IsNullOrWhiteSpace kv.Key then
            error "validate_menu_variables" "Menu variable name is empty." [] |> invalidOp
        elif kv.Key.Length > max_menu_variable_name_length then
            error "validate_menu_variables" "Menu variable name is too long." ["menu_variable_name", kv.Key; "maximum_menu_variable_name_length", max_menu_variable_name_length] |> invalidOp
    )

let private validate_runner_saveable_state
    (saved_state : Runner_Saveable_State)
    : unit =

    let validate_javascript_state (javascript_state : string) : unit =
        if String.IsNullOrWhiteSpace javascript_state then
            error "validate_runner_saveable_state" "JavaScript state is empty." [] |> invalidOp
(* We do not check the length of the JavaScript state here because we already check the saved game state length. *)
        else validate_javascript_state_json javascript_state

    match saved_state with
    | Runner_Saveable_State_Running data ->
        validate_javascript_state data.component_data.javascript_state
        validate_menu_variables data.menu_variables
    | Runner_Saveable_State_Done data ->
        validate_javascript_state data.javascript_state

let parse_and_validate_saved_state
    (saved_game_state : string)
    : Runner_Saveable_State =

    if String.IsNullOrWhiteSpace saved_game_state then
        error "parse_and_validate_saved_state" "Saved game state cannot be empty." [] |> invalidOp
    elif saved_game_state.Length > max_saved_game_state_length then
        error "parse_and_validate_saved_state" "Saved game state is too large." ["game_state_size_in_characters", saved_game_state.Length; "max_game_state_size_in_characters", max_saved_game_state_length] |> invalidOp
    else
        match Decode.Auto.fromString<Runner_Saveable_State> saved_game_state with
        | Ok saved_state ->
            validate_runner_saveable_state saved_state
            saved_state
        | Error parse_error ->
            error "parse_and_validate_saved_state" "Failed to deserialize saved game state." ["error", parse_error] |> invalidOp
