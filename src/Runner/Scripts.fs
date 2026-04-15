module Scripts

// String
open System

// console, window
open Browser.Dom
// jsNative
open Fable.Core
// Decode
open Thoth.Json

open Character_Types
open Command_Types
open Log
open Save_Load_Types
open Units_Of_Measure
open Utilities

(* Consts *)

[<Literal>]
let scripts_path = "../0_data/scenes/*"
[<Literal>]
let types_path = "../0_data/ts/*"
[<Literal>]
let backgrounds_path = "../0_data/bgs.txt?raw"
[<Literal>]
let characters_path = "../0_data/chars.txt?raw"
[<Literal>]
let music_path = "../0_data/music.txt?raw"
[<Literal>]
let database_configuration_path = "../0_data/db.txt?raw"

let entry_script_name = "start.txt"
let entry_scene_name = "start"
let entry_scene_id = 0<scene_id>
let temporary_scene_id_for_eval_command = -1<scene_id>
let scene_initial_command_id = 0<command_id>

let javascript_state_name = "state"
(* This is used by JavaScript_Interop_2.set_state_in_js (). We define it close to javascript_state_name so that if we change javascript_state_name, we won't forget to change set_javascript_state_emit accordingly. We can't insert a variable into a constant expression, and we need a constant expression to define an attribute. *)
[<Literal>]
let set_javascript_state_emit = "window.state = JSON.parse($0);"

(* See Parser_1_Grammar.get_grammar_text (). *)
let keywords =
    [
        "elif"
        "else"
        "elseif"
        "endif"
        "endimagemap"
        "endjs"
        "endmenu"
        "endnotify"
        "endstatus"
        "fadein"
        "fadeout"
        "fadeoutall"
        "fadeto"
        "hidedialogue"
        "hideimagemap"
        "if"
        "imagemap"
        "js"
        "jump"
        "menu"
        "notify"
        "playmusic"
        "showdialogue"
        "status"
        "stopmusic"
    ]

(* Debug *)

let debug_module_name = "Scripts"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Functions - helper *)

[<ImportDefault(database_configuration_path)>]
let private database_configuration_1 : string = jsNative

[<ImportDefault(backgrounds_path)>]
let private backgrounds_1 : string = jsNative

[<ImportDefault(characters_path)>]
let private characters_1 : string = jsNative

[<ImportDefault(music_path)>]
let private music_1 : string = jsNative

[<Emit("import.meta.globEager($0, { as: 'raw' })")>]
let private vite_glob_eager_raw (pattern: string) : obj = jsNative

let private convert_file_path_to_scene_name (path : string) =
    let fileName = path.Split '/' |> Array.last
    match fileName.LastIndexOf '.' with
    | -1 -> fileName
    | idx -> fileName.Substring (0, idx)

let private sprites_decoder : Decoder<Map<string,string>> =
    Decode.keyValuePairs Decode.string
    |> Decode.map Map.ofList

let private character_decoder : Decoder<Character_Input> =
    Decode.object (fun get ->
        { full_name  = get.Required.Field "full_name" Decode.string
          short_name = get.Required.Field "short_name" Decode.string
          height     = get.Required.Field "height" Decode.int |> LanguagePrimitives.Int32WithMeasure
          sprites      = get.Required.Field "sprites" sprites_decoder })

let private characters_decoder = Decode.list character_decoder

let private validate_database_configuration (database_configuration : Database_Configuration) : unit =
    if not <| is_valid_name database_configuration.database_name then
        error "validate_database_configuration" $"Database name must be non-empty and use only valid characters: {valid_name_characters}." ["invalid_database_name", database_configuration.database_name] |> invalidOp
    elif not <| is_valid_name database_configuration.store_name then
        error "validate_database_configuration" $"Store name must be non-empty and use only valid characters: {valid_name_characters}." ["invalid_store_name", database_configuration.store_name] |> invalidOp

let private validate_background_inputs (backgrounds : {| name : string; url : string |} list) : unit =
    backgrounds |> List.iter (fun entry ->
        if not <| is_valid_name entry.name then
            error "validate_background_inputs" $"Background names must be non-empty and use only valid characters: {valid_name_characters}." ["invalid_background_name", entry.name] |> invalidOp
    )

let private validate_music_inputs (music : {| name : string; url : string |} list) : unit =
    music |> List.iter (fun entry ->
        if not <| is_valid_name entry.name then
            error "validate_music_inputs" $"Music names must be non-empty and use only valid characters: {valid_name_characters}." ["invalid_music_name", entry.name] |> invalidOp
    )

let private validate_character_inputs (characters : Character_Input list) : unit =
    characters |> List.iter (fun character ->
        if not <| is_valid_name character.short_name then
            error "validate_character_inputs" $"Character short names must be non-empty and use only valid characters: {valid_name_characters}." ["invalid_character", character] |> invalidOp
        elif List.contains character.short_name keywords then
            error "validate_character_inputs" "Character short name collides with a keyword." ["character", character; "keywords", keywords] |> invalidOp
    )

(* Functions - main *)

let get_database_configuration () : Database_Configuration =
    match Decode.Auto.fromString<Database_Configuration> database_configuration_1 with
    | Ok database_configuration_2 ->
        do validate_database_configuration database_configuration_2
        database_configuration_2
    | Error message -> error "get_database_configuration" "Failed to deserialize database configuration." ["database_configuration", database_configuration_1; "error_message", message] |> invalidOp

let get_backgrounds () : Map<string, string> =
    match Decode.Auto.fromString<{| name : string; url : string |} list> backgrounds_1 with
    | Ok backgrounds_2 ->
        do validate_background_inputs backgrounds_2
        backgrounds_2 |> List.map (fun entry -> entry.name, entry.url) |> Map.ofList
    | Error message -> error "get_backgrounds" "Failed to deserialize backgrounds." ["backgrounds", backgrounds_1; "error_message", message] |> invalidOp

let get_character_inputs () : Character_Input_Map =
    match Decode.fromString characters_decoder characters_1 with
    | Ok characters_2 ->
        do validate_character_inputs characters_2    
        characters_2 |> List.map (fun character -> character.short_name, character) |> Map.ofList
    | Error message -> error "get_character_inputs" "Failed to deserialize characters." ["characters", characters_1; "error_message", message] |> invalidOp

let get_music () : Map<string, string> =
    match Decode.Auto.fromString<{| name : string; url : string |} list> music_1 with
    | Ok music_2 ->
        do validate_music_inputs music_2
        music_2 |> List.map (fun entry -> entry.name, entry.url) |> Map.ofList
    | Error message -> error "get_music" "Failed to deserialize music." ["music", music_1; "error_message", message] |> invalidOp

let get_scripts () : Script list =
(* ID 0 is reserved for the entry scene. *)
    let mutable scene_id = 1
    let mutable entry_scene_found = false

    let scripts =
        vite_glob_eager_raw scripts_path
        |> fun result -> JS.Constructors.Object.entries result
        |> unbox<(string * string)[]>
        |> Array.toList
(* Convert the scripts to scene data. *)
        |> List.map (fun (path, content) ->
                let scene_name = convert_file_path_to_scene_name path
                {
                    id =
                        if 0 = String.Compare (entry_scene_name, scene_name) then
                            do entry_scene_found <- true
                            entry_scene_id
                        else
                            let result = LanguagePrimitives.Int32WithMeasure <| scene_id 
                            do scene_id <- scene_id + 1
                            result
                    name = scene_name
                    content = content
                }
            )

(* Make sure the scripts include the expected entry scene. *)
    if not entry_scene_found then
        error "get_scripts" "Entry script missing." ["Expected entry script name", entry_script_name; "Script names", scripts |> List.map (fun script -> script.name) :> obj] |> invalidOp
    else scripts

let get_typescript_types () : string =
    let results =
        vite_glob_eager_raw types_path
        |> fun result -> JS.Constructors.Object.entries result
        |> unbox<(string * string)[]>
        |> Seq.map (fun (path, content) -> $"// {path}{Environment.NewLine}{Environment.NewLine}{content}")
    String.Join (Environment.NewLine, results)

(* TODO2 Should we automatically fade out the dialogue box during a scene change? Should we fade it in for the first line of dialogue in a scene? Or should these things be left to the author?
For now, we simply show the dialogue box whenever there is a dialogue command and the dialogue box is not already visible.
We also added the fade_out_all command to fade out background and characters and hide the dialogue box, and authors are supposed to use this before jumping to another scene.
*)
