module Parser_1_Match_Patterns

// Double, Int32
open System
// Regex
open System.Text.RegularExpressions

open Character_Types
open Command_Types
open Fade_Types
open Menu
open Log

(* Debug *)

let debug_module_name = "Parser_1_Match_Patterns"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts - patterns *)

let get_compiled_regex (pattern : string) = Regex (pattern, RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

let single_line_comment_regex = @"^//.*$" |> get_compiled_regex
let multi_line_comment_start_regex = @"^/\*.*$" |> get_compiled_regex
let multi_line_comment_end_regex = @".*?\*/$" |> get_compiled_regex
let private music_play_regex = @"^play\s+(\S+)$" |> get_compiled_regex
let private music_stop_regex = @"^stop$" |> get_compiled_regex
let private background_fade_in_regex = @"^fadein\s+(\S+)\s+([\d\.]+)$" |> get_compiled_regex
let private background_fade_out_regex = @"^fadeout\s+([\d\.]+)$" |> get_compiled_regex
let private background_cross_fade_regex = @"^fadeto\s+(\S+)\s+([\d\.]+)$" |> get_compiled_regex
let private dialogue_regex = @"^(\w+)\s+(.+)$" |> get_compiled_regex
let private fade_in_character_regex = @"^fadein\s+(\w+)\s+(\w+)\s+(\d+)\s+([\d\.]+)$" |> get_compiled_regex
let private fade_out_character_regex = @"^fadeout\s+(\w+)\s+([\d\.]+)$" |> get_compiled_regex
let private cross_fade_character_regex = @"^fadeto\s+(\w+)\s+(\w+)\s+([\d\.]+)$" |> get_compiled_regex
let private fade_out_all_regex = @"^fadeoutall\s+([\d\.]+)$" |> get_compiled_regex
let private dialogue_box_show_regex = @"^show dialogue box$" |> get_compiled_regex
let private dialogue_box_hide_regex = @"^hide dialogue box$" |> get_compiled_regex

let private javascript_inline_regex = @"^js\s+(.+)$" |> get_compiled_regex
(* We use curly braces to delimit interpolations, so the author cannot, for instance, write an interpolation that contains the following.
function (x) { return x; }
(end)
They will need to define functions elsewhere, but they can call them here.
*)
let private javascript_interpolation_regex = @"\{([^}]+)\}" |> get_compiled_regex
let private if_start_regex = @"^if\s+(.+)$" |> get_compiled_regex
let private end_if_regex = @"^endif$" |> get_compiled_regex
let private else_if_regex = @"^elseif\s+(.+)$" |> get_compiled_regex
let private else_regex = @"^else$" |> get_compiled_regex
let private jump_regex = @"^jump\s+([\w-]+)$" |> get_compiled_regex

(* These are used in Parser_1_Match_Functions.fs. *)
let javascript_start_regex = @"^js$" |> get_compiled_regex
let javascript_end_regex = @"^endjs$" |> get_compiled_regex
let menu_start_regex = @"^menu\s+(\w+)\s+(.+)$" |> get_compiled_regex
let menu_item_regex = @"^(\d+)\s+([^\$]+?)(?:\s+(\$if\s+(.+)))?$" |> get_compiled_regex
let end_menu_regex = @"^endmenu$" |> get_compiled_regex

(* Helper functions - patterns *)

let private extract_javascript_interpolations (input : string) : string list =
    let matches = javascript_interpolation_regex.Matches input
    [ for m in matches -> m.Groups.[1].Value ]

// TODO2 Consider just letting authors use ${} for JS interpolation. Then we can use {} for other purposes.
let private convert_string_to_use_javascript_interpolation (text_1 : string) : string =
    let text_2 = text_1.Replace ("{", "${")
    $"`{text_2}`"

(* Helper functions - matching *)

let match_music_play (text : string) : Command_Pre_Parse option =
    let m = music_play_regex.Match text
    if m.Success then Command_Types.Music_Play m.Groups.[1].Value |> Command_Pre_Parse.Command |> Some
    else None

let match_music_stop (text : string) : Command_Pre_Parse option =
    let m = music_stop_regex.Match text
    if m.Success then Command_Types.Music_Stop |> Command_Pre_Parse.Command |> Some
    else None

let match_background_fade_in (text : string) (backgrounds : Map<string, string>) : Command_Pre_Parse option =
    let m = background_fade_in_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[2].Value with
        | true, transition_time ->
            let background = m.Groups[1].Value
            match backgrounds.TryFind background with
            | Some url ->
                Command_Types.Background_Fade_In {
                    new_url = url
                    transition_time = LanguagePrimitives.FloatWithMeasure transition_time
                } |> Command_Pre_Parse.Command |> Some
            | None -> error "match_background_fade_in" "Unknown background." ["background", background; "backgrounds", backgrounds] |> invalidOp
        | _ -> None
    else None

let match_background_fade_out (text : string) : Command_Pre_Parse option =
    let m = background_fade_out_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[1].Value with
        | true, transition_time ->
            Command_Types.Background_Fade_Out {
                transition_time = LanguagePrimitives.FloatWithMeasure transition_time
            } |> Command_Pre_Parse.Command |> Some
        | _ -> None
    else None

let match_background_cross_fade (text : string) (backgrounds : Map<string, string>) : Command_Pre_Parse option =
    let m = background_cross_fade_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[2].Value with
        | true, transition_time ->
            let background = m.Groups[1].Value
            match backgrounds.TryFind background with
            | Some url ->
                Command_Types.Background_Cross_Fade {
                    new_url = url
                    transition_time = LanguagePrimitives.FloatWithMeasure transition_time
                } |> Command_Pre_Parse.Command |> Some
            | None -> error "match_background_cross_fade" "Unknown background." ["background", background; "backgrounds", backgrounds] |> invalidOp
        | _ -> None
    else None

let match_character_fade_in (text : string) (characters : Character_Input_Map) : Command_Pre_Parse option =
    let m = fade_in_character_regex.Match text
    if m.Success then
        match Int32.TryParse m.Groups[3].Value with
        | true, position ->
            match Double.TryParse m.Groups[4].Value with
            | true, transition_time ->
                let character_short_name, mood = m.Groups[1].Value, m.Groups[2].Value
                match characters.TryFind character_short_name with
                | Some character ->
                    match character.moods.TryFind mood with
                    | Some url ->
                        Command_Types.Character_Fade_In {
                            character_short_name = character_short_name
                            url = url
                            position = LanguagePrimitives.Int32WithMeasure position
                            transition_time = LanguagePrimitives.FloatWithMeasure transition_time
                        } |> Command_Pre_Parse.Command |> Some
                    | None -> error "match_character_fade_in" "Unknown mood." ["character_full_name", character.full_name; "character", character; "mood", mood] |> invalidOp
                | None -> error "match_character_fade_in" "Unknown character." ["character_short_name", character_short_name; "characters", characters] |> invalidOp
            | _ -> None
        | _ -> None
    else None

let match_character_fade_out (text : string) (characters : Character_Input_Map): Command_Pre_Parse option =
    let m = fade_out_character_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[2].Value with
        | true, transition_time ->
            let character_short_name = m.Groups[1].Value
            if characters.ContainsKey character_short_name then
                Command_Types.Character_Fade_Out {
                    character_short_name = character_short_name
                    transition_time = LanguagePrimitives.FloatWithMeasure transition_time
                } |> Command_Pre_Parse.Command |> Some
            else error "match_character_fade_out" "Unknown character." ["character_short_name", character_short_name; "characters", characters] |> invalidOp
        | _ -> None
    else None

let match_character_cross_fade (text : string) (characters : Character_Input_Map) : Command_Pre_Parse option =
    let m = cross_fade_character_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[3].Value with
        | true, transition_time ->
            let character_short_name, mood = m.Groups[1].Value, m.Groups[2].Value
            match characters.TryFind character_short_name with
            | Some character ->
                match character.moods.TryFind mood with
                | Some url ->
                    Command_Types.Character_Cross_Fade {
                        character_short_name = character_short_name
                        url = url
                        transition_time = LanguagePrimitives.FloatWithMeasure transition_time
                    } |> Command_Pre_Parse.Command |> Some
                | None -> error "match_character_cross_fade" "Unknown mood." ["character_full_name", character.full_name; "character", character; "mood", mood] |> invalidOp
            | None -> error "match_character_cross_fade" "Unknown character." ["character_short_name", character_short_name; "characters", characters] |> invalidOp
        | _ -> None
    else None

let match_fade_out_all (text : string) : Command_Pre_Parse option =
    let m = fade_out_all_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[1].Value with
        | true, transition_time ->
            transition_time |> LanguagePrimitives.FloatWithMeasure |> Fade_Out_All |> Command_Pre_Parse.Command |> Some
        | _ -> None
    else None

let match_dialogue_box_show (text : string) : Command_Pre_Parse option =
    let m = dialogue_box_show_regex.Match text
    if m.Success then Dialogue_Box_Show |> Command_Pre_Parse.Command |> Some
    else None

let match_dialogue_box_hide (text : string) : Command_Pre_Parse option =
    let m = dialogue_box_hide_regex.Match text
    if m.Success then Dialogue_Box_Hide |> Command_Pre_Parse.Command |> Some
    else None

let match_dialogue (text : string) (characters : Character_Input_Map) : Command_Pre_Parse option =
    let m = dialogue_regex.Match text
    if m.Success then
        let character_short_name, text = m.Groups[1].Value, m.Groups[2].Value
        match characters.TryFind character_short_name with
        | Some character ->
            Dialogue {
                character_short_name = character_short_name
                character_full_name = character.full_name
                text = text |> convert_string_to_use_javascript_interpolation
                javascript_interpolations = extract_javascript_interpolations text
            } |> Command_Pre_Parse.Command |> Some
        | None -> error "match_dialogue" "Unknown character." ["character_short_name", character_short_name; "characters", characters] |> invalidOp
    else None

let match_javascript_inline (text : string) : Command_Pre_Parse option =
    let m = javascript_inline_regex.Match text
    if m.Success then m.Groups[1].Value |> JavaScript_Inline |> Command_Pre_Parse.Command |> Some
    else None

let match_if_start (text : string) : Command_Pre_Parse option =
    let m = if_start_regex.Match text
    if m.Success then m.Groups[1].Value |> Command_Pre_Parse.If |> Some
    else None

let match_if_end (text : string) : Command_Pre_Parse option =
    let m = end_if_regex.Match text
    if m.Success then Command_Pre_Parse.End_If |> Some
    else None

let match_else_if (text : string) : Command_Pre_Parse option =
    let m = else_if_regex.Match text
    if m.Success then m.Groups[1].Value |> Command_Pre_Parse.Else_If |> Some
    else None

let match_else (text : string) : Command_Pre_Parse option =
    let m = else_regex.Match text
    if m.Success then Command_Pre_Parse.Else |> Some
    else None

let match_menu_start (text : string) : Menu_Data option =
    let m = menu_start_regex.Match text
    if m.Success then
        let description = m.Groups[2].Value
        {
            name = m.Groups[1].Value
            description = description |> convert_string_to_use_javascript_interpolation
            items = []
            javascript_interpolations = extract_javascript_interpolations description
        } |> Some
    else None

let match_menu_item (text : string) : Menu_Item_Data option =
    let m = menu_item_regex.Match text
    if m.Success then
        let text = m.Groups[2].Value
        {
            value = Int32.Parse m.Groups[1].Value
            text = text |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text
(* If the pattern defines an optional group, that group is present in m.Groups even if it was not matched in the input text. *)
            conditional = if m.Groups[4].Success then m.Groups[4].Value |> Some else None
        } |> Some
    else None

let match_jump (text : string) (scripts : Script list) : Command_Pre_Parse option =
    let m = jump_regex.Match text
    if m.Success then
        let destination = m.Groups[1].Value
        match scripts |> List.tryFind (fun script -> script.name = destination) with
        | Some script -> script.id |> Jump |> Command_Pre_Parse.Command |> Some
        | None -> error "match_jump" "Jump destination not found." ["destination", destination; "scripts", scripts] |> invalidOp
    else None
