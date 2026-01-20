module Parser_1_Helpers

open System.Text.RegularExpressions

// console, window
open Browser.Dom

open Character_Types
open Command_Types
open Log
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Parser_1_Helpers"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Helper functions - javascript. These are used to parse all commands that allow javascript interpolation. *)

let get_compiled_regex (pattern : string) = Regex (pattern, RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

let javascript_interpolation_regex = @"\$\{(?<interpolation>[^}]+)\}" |> get_compiled_regex

let extract_javascript_interpolations (input : string) : string list =
    let matches = javascript_interpolation_regex.Matches input
    [ for m in matches -> m.Groups.["interpolation"].Value ]

let convert_string_to_use_javascript_interpolation (text : string) : string = $"`{text}`"

(* Helper functions - matched_command_to_pre_parse_command *)

// TODO1 #parsing For each of these, add a line_number parameter.
let get_music
    (music_tracks : Map<string, string>)
    (track_name : string)
    : string =

    match music_tracks.TryFind track_name with
    | Some url -> url
    | None -> error "get_music" "Unknown music track name." ["music track name", track_name; "known music tracks", music_tracks] |> invalidOp

let get_background
    (backgrounds : Map<string, string>)
    (background_name : string)
    : string =

    match backgrounds.TryFind background_name with
    | Some url -> url
    | None -> error "get_background" "Unknown background name." ["background name", background_name; "known backgrounds", backgrounds] |> invalidOp

let get_character
    (characters : Character_Input_Map)
    (character_short_name : string)
    : Character_Input =

    match characters.TryFind character_short_name with
    | Some character -> character
    | None -> error "get_character" "Unknown character." ["character_short_name", character_short_name; "known characters", characters] |> invalidOp

let get_character_sprite
    (characters : Character_Input_Map)
    (character_short_name : string)
    (character_sprite_name : string)
    : string =

    let character = get_character characters character_short_name
    match character.sprites.TryFind character_sprite_name with
    | Some url -> url
    | None -> error "get_character_sprite" "Unknown character sprite." ["sprite", character_sprite_name; "character data", character] |> invalidOp

let get_script_id
    (scripts : Script list)
    (destination : string)
    : int<scene_id> =

    match scripts |> List.tryFind (fun script -> script.name = destination) with
    | Some script -> script.id
    | None -> error "get_script_id" "Jump destination not found." ["destination", destination; "scripts", scripts] |> invalidOp
