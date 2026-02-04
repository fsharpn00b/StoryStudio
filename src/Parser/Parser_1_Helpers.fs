module Parser_1_Helpers

// Environment.NewLine, Exception, String
open System
// Regex
open System.Text.RegularExpressions

// console, window
open Browser.Dom

open Character_Types
open Command_Types
open Image_Map
open Log
open Menu
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Parser_1_Helpers"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

(* TODO1 #exceptions As in JavaScript_Interop_1, explain we raise a custom error type because we still need to gather more data, namely the script name and line number that caused the error. Those data are not available to the semantics functions.
*)
type Parsing_Semantics_Error_Data_1 = {
    message : string
    script_text_index : int
    data : (string * obj) list
}
exception Parsing_Semantics_Error_1 of Parsing_Semantics_Error_Data_1

type Parsing_Semantics_Error_Data_2 = {
    message : string
    script_name : string
    script_line_number : int
    data : (string * obj) list
}
exception Parsing_Semantics_Error_2 of Parsing_Semantics_Error_Data_2

(* Helper functions - javascript. These are used to parse all commands that allow javascript interpolation. *)

let private get_compiled_regex (pattern : string) = Regex (pattern, RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

let private javascript_interpolation_regex = @"\$\{(?<interpolation>[^}]+)\}" |> get_compiled_regex

let extract_javascript_interpolations (input : string) : string list =
    let matches = javascript_interpolation_regex.Matches input
    [ for m in matches -> m.Groups.["interpolation"].Value ]

let convert_string_to_use_javascript_interpolation (text : string) : string = $"`{text}`"

(* Helper functions *)

let get_script_line_number
    (script_text : string)
    (script_text_index: int)
    : int =
    Regex.Matches(script_text.Substring (0, script_text_index), Regex.Escape Environment.NewLine).Count + 1

(* We cannot wait to get the script_text_index from Command_Pre_Parse, because these functions are called to help create a Command_Pre_Parse instance. *)
let get_music_track_url
    (music_tracks : Map<string, string>)
    (track_name : string)
    (script_text_index : int)
    : string =

    match music_tracks.TryFind track_name with
    | Some url -> url
    | None ->
        {
            message = "Unknown music track name."
            script_text_index = script_text_index
            data = ["music track name", track_name; "known music tracks", music_tracks]
        } |> Parsing_Semantics_Error_1 |> raise

let get_background_url
    (backgrounds : Map<string, string>)
    (background_name : string)
    (script_text_index : int)
    : string =

    match backgrounds.TryFind background_name with
    | Some url -> url
    | None ->
        {
            message = "Unknown background name."
            script_text_index = script_text_index
            data = ["background name", background_name; "known backgrounds", backgrounds]
        } |> Parsing_Semantics_Error_1 |> raise

let get_character_input_data
    (characters : Character_Input_Map)
    (character_short_name : string)
    (script_text_index : int)
    : Character_Input =

    match characters.TryFind character_short_name with
    | Some character -> character
    | None ->
        {
            message = "Unknown character short name."
            script_text_index = script_text_index
            data = ["character_short_name", character_short_name; "known characters", characters]
        } |> Parsing_Semantics_Error_1 |> raise

let get_character_sprite_url
    (characters : Character_Input_Map)
    (character_short_name : string)
    (character_sprite_name : string)
    (script_text_index : int)
    : string =

    let character = get_character_input_data characters character_short_name script_text_index
    match character.sprites.TryFind character_sprite_name with
    | Some url -> url
    | None ->
        {
            message = "Unknown character sprite name."
            script_text_index = script_text_index
            data = ["sprite", character_sprite_name; "character data", character]
        } |> Parsing_Semantics_Error_1 |> raise

let get_script_id
    (scripts : Script list)
    (destination : string)
    (script_text_index : int)
    : int<scene_id> =

    match scripts |> List.tryFind (fun script -> script.name = destination) with
    | Some script -> script.id
    | None ->
        {
            message = "Jump destination name not found."
            script_text_index = script_text_index
            data = ["destination_name", destination; "Known destinations", scripts |> List.map (fun script -> script.name) :> obj]
        } |> Parsing_Semantics_Error_1 |> raise

let get_move_in_semantics
    (characters : Character_Input_Map)
    (character_short_name : string) 
    (character_sprite_name : string)
    (direction : string)
    (position : int<percent>)
    (transition_time : Transition_Time)
    (script_text_index : int)
    : Command_Pre_Parse_Type =

    {
        Character_Move_In_Data.character_short_name = character_short_name
        url = get_character_sprite_url characters character_short_name character_sprite_name script_text_index
        direction =
            match direction with
            | "left" -> Character_Move_Direction.Left
            | "right" -> Character_Move_Direction.Right
// TODO2 #transitions Add this.
//            | "bottom" -> Character_Move_Direction.Bottom
            | _ ->
                {
                    message = "Unknown character move in direction."
                    script_text_index = script_text_index
                    data = ["direction", direction]
                } |> Parsing_Semantics_Error_1 |> raise
        position = position
        transition_time = transition_time
    } |> Character_Move_Data_2.In |> Character_Move |> Command_Pre_Parse_Type.Command

let get_move_out_semantics
    (character_short_name : string) 
    (direction : string)
    (transition_time : Transition_Time)
    (script_text_index : int)
    : Command_Pre_Parse_Type =

    {
        Character_Move_Out_Data.character_short_name = character_short_name
        direction =
            match direction with
            | "left" -> Character_Move_Direction.Left
            | "right" -> Character_Move_Direction.Right
// TODO2 #transitions Add this.
//            | "bottom" -> Character_Move_Direction.Bottom
            | _ ->
                {
                    message = "Unknown character move out direction."
                    script_text_index = script_text_index
                    data = ["direction", direction]
                } |> Parsing_Semantics_Error_1 |> raise
        transition_time = transition_time
    } |> Character_Move_Data_2.Out |> Character_Move |> Command_Pre_Parse_Type.Command

(* These checks either would be too complex to do with the grammar (for example, a menu could contain only comments, yet appear non-empty) or detect errors for which we prefer more descriptive error messages. *)
let check_menu_items
    (menu_name : string)
    (items_1 : Menu_Item_Data_1 list)
    (script_text_index : int)
    : unit =

    if Seq.isEmpty items_1 then
        {
            message = "Menu must contain at least one item with no conditional."
            script_text_index = script_text_index
            data = ["menu_name", menu_name]
        } |> Parsing_Semantics_Error_1 |> raise
    else
        let items_2 = items_1 |> Seq.filter (fun item -> item.conditional.IsNone)
        if Seq.isEmpty items_2 then
            {
                message = "Menu must contain at least one item with no conditional."
                script_text_index = script_text_index
                data = ["menu_name", menu_name]
            } |> Parsing_Semantics_Error_1 |> raise

let check_image_map_items
    (image_map_name : string)
    (items_1 : Image_Map_Item_Data list)
    (script_text_index : int)
    : unit =

    if Seq.isEmpty items_1 then
        {
            message = "Image map must contain at least one item."
            script_text_index = script_text_index
            data = ["image_map_name", image_map_name]
        } |> Parsing_Semantics_Error_1 |> raise
    else
        let items_2 = items_1 |> Seq.filter (fun item -> item.conditional.IsNone)
        if Seq.isEmpty items_2 then
            {
                message = "Image map must contain at least one item with no conditional."
                script_text_index = script_text_index
                data = ["image_map_name", image_map_name]
            } |> Parsing_Semantics_Error_1 |> raise
