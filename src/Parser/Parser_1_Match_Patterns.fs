module Parser_1_Match_Patterns

// Double, Int32
open System
// Regex
open System.Text.RegularExpressions

open Character_Types
open Command_Types
open Image_Map
open Log
open Menu
open Temporary_Notification
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Parser_1_Match_Patterns"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts - patterns *)

let get_compiled_regex (pattern : string) = Regex (pattern, RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

(* TODO2 This will also match an int. So if we want to try to match either a float or int, we should try to match an int first. *)
let float_regex = @"[0-9]+(\.[0-9]+)?"

// TODO1 To improve matching, use text.StartsWith () to get the command, then use regex to match the rest. That makes matching less brittle. Even if the author formats the parameters wrong, we still know what command they were using, and can give them more useful error messages.
// TODO1 Keep a list of reserved words so the author cannot name characters after them.
let single_line_comment_regex = @"^//.*$" |> get_compiled_regex
let multi_line_comment_start_regex = @"^/\*.*$" |> get_compiled_regex
let multi_line_comment_end_regex = @".*?\*/$" |> get_compiled_regex
let private music_play_regex = @"^play\s+(\S+)$" |> get_compiled_regex
let private music_stop_regex = @"^stop$" |> get_compiled_regex
let private background_fade_in_regex = @$"^fadein\s+(\S+)\s+({float_regex})$" |> get_compiled_regex
let private background_fade_out_regex = @$"^fadeout\s+({float_regex})$" |> get_compiled_regex
let private background_cross_fade_regex = @$"^fadeto\s+(\S+)\s+({float_regex})$" |> get_compiled_regex
let private fade_in_character_regex = @$"^fadein\s+(\w+)\s+(\w+)\s+(\d+)\s+({float_regex})$" |> get_compiled_regex
let private fade_out_character_regex = @$"^fadeout\s+(\w+)\s+({float_regex})$" |> get_compiled_regex
let private cross_fade_character_regex = @$"^fadeto\s+(\w+)\s+(\w+)\s+({float_regex})$" |> get_compiled_regex
let private fade_out_all_regex = @$"^fadeoutall\s+({float_regex})$" |> get_compiled_regex
let private dialogue_box_show_regex = @"^show dialogue box$" |> get_compiled_regex
let private dialogue_box_hide_regex = @"^hide dialogue box$" |> get_compiled_regex
let private dialogue_regex = @"^(\w+)\s+(.+)$" |> get_compiled_regex
let private temporary_notification_regex = @$"^notify\s+(.+)$" |> get_compiled_regex
let private permanent_notification_regex = @$"^status\s+(.+)$" |> get_compiled_regex

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
(* \w includes _, but not -. *)
let private jump_regex = @"^jump\s+([\w-]+)$" |> get_compiled_regex

(* These are used in Parser_1_Match_Functions.fs. *)
let javascript_start_regex = @"^js$" |> get_compiled_regex
let javascript_end_regex = @"^endjs$" |> get_compiled_regex
let menu_start_regex = @"^menu\s+(\w+)\s+(.+)$" |> get_compiled_regex
let menu_item_regex = @"^(\d+)\s+([^\$]+?)(?:\s+(\$if\s+(.+)))?$" |> get_compiled_regex
let end_menu_regex = @"^endmenu$" |> get_compiled_regex
let image_map_start_regex = @$"^imagemap\s+(\w+)\s+(\w+)\s+({float_regex})$" |> get_compiled_regex
let image_map_item_regex = @"^(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)(?:\s+(\$if\s+(.+)))?$" |> get_compiled_regex
let end_image_map_regex = @$"^endimagemap\s+({float_regex})$" |> get_compiled_regex

(* Helper functions - patterns *)

let private extract_javascript_interpolations (input : string) : string list =
    let matches = javascript_interpolation_regex.Matches input
    [ for m in matches -> m.Groups.[1].Value ]

// TODO2 Consider just letting authors use ${} for JS interpolation. Then we can use {} for other purposes.
let private convert_string_to_use_javascript_interpolation (text_1 : string) : string =
    let text_2 = text_1.Replace ("{", "${")
    $"`{text_2}`"

(* Helper functions - matching *)

let match_music_play (text : string) (music : Map<string, string>) : Command_Pre_Parse option =
    let m = music_play_regex.Match text
    if m.Success then
        match music.TryFind m.Groups.[1].Value with
        | Some url -> Command_Types.Music_Play url |> Command_Pre_Parse.Command |> Some
        | None -> error "match_music_play" "Unknown music name." ["music name", m.Groups[1].Value; "known music names", music] |> invalidOp
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
            | None -> error "match_background_fade_in" "Unknown background." ["background", background; "known backgrounds", backgrounds] |> invalidOp
        | _ -> error "match_background_fade_in" "Failed to parse transition time." ["transition_time", m.Groups[2].Value] |> invalidOp
    else None

let match_background_fade_out (text : string) : Command_Pre_Parse option =
    let m = background_fade_out_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[1].Value with
        | true, transition_time ->
            Command_Types.Background_Fade_Out {
                transition_time = LanguagePrimitives.FloatWithMeasure transition_time
            } |> Command_Pre_Parse.Command |> Some
        | _ -> error "match_background_fade_out" "Failed to parse transition time." ["transition_time", m.Groups[1].Value] |> invalidOp
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
            | None -> error "match_background_cross_fade" "Unknown background." ["background", background; "known backgrounds", backgrounds] |> invalidOp
        | _ -> error "match_background_cross_fade" "Failed to parse transition time." ["transition_time", m.Groups[2].Value] |> invalidOp
    else None

let match_character_fade_in (text : string) (characters : Character_Input_Map) : Command_Pre_Parse option =
    let m = fade_in_character_regex.Match text
    if m.Success then
        match Int32.TryParse m.Groups[3].Value with
        | true, position ->
            match Double.TryParse m.Groups[4].Value with
            | true, transition_time ->
                let character_short_name, sprite = m.Groups[1].Value, m.Groups[2].Value
                match characters.TryFind character_short_name with
                | Some character ->
                    match character.sprites.TryFind sprite with
                    | Some url ->
                        Command_Types.Character_Fade_In {
                            character_short_name = character_short_name
                            url = url
                            position = LanguagePrimitives.Int32WithMeasure position
                            transition_time = LanguagePrimitives.FloatWithMeasure transition_time
                        } |> Command_Pre_Parse.Command |> Some
                    | None -> error "match_character_fade_in" "Unknown character sprite." ["character_full_name", character.full_name; "character", character; "sprite", sprite; "known sprites for this character", character.sprites] |> invalidOp
                | None -> error "match_character_fade_in" "Unknown character." ["character_short_name", character_short_name; "known characters", characters] |> invalidOp
            | _ -> error "match_character_fade_in" "Failed to parse transition time." ["transition_time", m.Groups[4].Value] |> invalidOp
        | _ -> error "match_character_fade_in" "Failed to parse position." ["position", m.Groups[3].Value] |> invalidOp
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
            else error "match_character_fade_out" "Unknown character." ["character_short_name", character_short_name; "known characters", characters] |> invalidOp
        | _ -> error "match_character_fade_out" "Failed to parse transition time." ["transition_time", m.Groups[2].Value] |> invalidOp
    else None

let match_character_cross_fade (text : string) (characters : Character_Input_Map) : Command_Pre_Parse option =
    let m = cross_fade_character_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[3].Value with
        | true, transition_time ->
            let character_short_name, sprite = m.Groups[1].Value, m.Groups[2].Value
            match characters.TryFind character_short_name with
            | Some character ->
                match character.sprites.TryFind sprite with
                | Some url ->
                    Command_Types.Character_Cross_Fade {
                        character_short_name = character_short_name
                        url = url
                        transition_time = LanguagePrimitives.FloatWithMeasure transition_time
                    } |> Command_Pre_Parse.Command |> Some
                | None -> error "match_character_cross_fade" "Unknown character sprite." ["character_full_name", character.full_name; "character", character; "sprite", sprite; "known sprites for this character", character.sprites] |> invalidOp
            | None -> error "match_character_cross_fade" "Unknown character." ["character_short_name", character_short_name; "known characters", characters] |> invalidOp
        | _ -> error "match_character_cross_fade" "Failed to parse transition time." ["transition_time", m.Groups[3].Value] |> invalidOp
    else None

let match_fade_out_all (text : string) : Command_Pre_Parse option =
    let m = fade_out_all_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[1].Value with
        | true, transition_time ->
            transition_time |> LanguagePrimitives.FloatWithMeasure |> Fade_Out_All |> Command_Pre_Parse.Command |> Some
        | _ -> error "match_fade_out_all" "Failed to parse transition time." ["transition_time", m.Groups[1].Value] |> invalidOp
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
                javascript_interpolations = text |> extract_javascript_interpolations
            } |> Command_Pre_Parse.Command |> Some
// TODO2 This error usually results from an unrecognized command, so we clarified the error message for that instead.
//        | None -> error "match_dialogue" "Unrecognized character." ["character_short_name", character_short_name; "characters", characters] |> invalidOp
        | None -> None
    else None

let match_temporary_notification (text : string) : Command_Pre_Parse option =
    let m = temporary_notification_regex.Match text
    if m.Success then
        let text = m.Groups[1].Value
        {
            text = text |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = text |> extract_javascript_interpolations
        } |> Temporary_Notification |> Command_Pre_Parse.Command |> Some
    else None

let match_permanent_notification (text : string) : Command_Pre_Parse option =
    let m = permanent_notification_regex.Match text
    if m.Success then
        let text = m.Groups[1].Value
        {
            text = text |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = text |> extract_javascript_interpolations
        } |> Permanent_Notification |> Command_Pre_Parse.Command |> Some
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

let match_menu_start (text : string) : Menu_Data_1 option =
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

let match_menu_item (text : string) : Menu_Item_Data_1 option =
    let m = menu_item_regex.Match text
    if m.Success then
        match Int32.TryParse m.Groups[1].Value with
        | true, value ->
            let text = m.Groups[2].Value
            {
                value = value
                text = text |> convert_string_to_use_javascript_interpolation
                javascript_interpolations = extract_javascript_interpolations text
(* If the pattern defines an optional group, that group is present in m.Groups even if it was not matched in the input text. *)
                conditional = if m.Groups[4].Success then m.Groups[4].Value |> Some else None
            } |> Some
        | _ -> error "match_menu_item" "Failed to parse menu item index." ["menu item index", m.Groups[1].Value] |> invalidOp
    else None

let match_image_map_start (text : string) (backgrounds : Map<string, string>) : Image_Map_Data option =
    let m = image_map_start_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[3].Value with
        | true, transition_time ->
            let background = m.Groups[2].Value
            match backgrounds.TryFind background with
            | Some url ->
                {
                    name = m.Groups[1].Value
                    url = url
                    items = []
                    transition_time = transition_time |> LanguagePrimitives.FloatWithMeasure
                } |> Some
            | None -> error "match_image_map_start" "Unknown image map background." ["background", background; "known backgrounds", backgrounds] |> invalidOp
        | _ -> error "match_image_map_start" "Failed to parse transition time." ["transition_time", m.Groups[3].Value] |> invalidOp
    else None

let match_image_map_end (text : string) : Fade_Transition_Time option =
    let m = end_image_map_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups[1].Value with
        | true, transition_time -> transition_time |> LanguagePrimitives.FloatWithMeasure |> Some
        | _ -> error "match_image_map_end" "Failed to parse transition time." ["transition_time", m.Groups[1].Value] |> invalidOp
    else None

let match_image_map_item (text : string) : Image_Map_Item_Data option =
    let m = image_map_item_regex.Match text
    if m.Success then
        match Int32.TryParse m.Groups[1].Value with
        | true, value ->
            match
                Int32.TryParse m.Groups[2].Value,
                Int32.TryParse m.Groups[3].Value,
                Int32.TryParse m.Groups[4].Value,
                Int32.TryParse m.Groups[5].Value
            with
            | (true, x1), (true, y1), (true, x2), (true, y2) ->
                {
                    value = value
                    x1 = x1
                    y1 = y1
                    x2 = x2
                    y2 = y2
// TODO2 Why would an image map item have a JS interpolation? There is no text. It can have a conditional, but not an interpolation. We think this was copied over from menu item. We could leave it in here for the author to show alt text (that can include JS interpolation).
                    javascript_interpolations = extract_javascript_interpolations text
(* If the pattern defines an optional group, that group is present in m.Groups even if it was not matched in the input text. *)
                    conditional = if m.Groups[7].Success then m.Groups[7].Value |> Some else None
                } |> Some
            | _ -> error "match_image_map_item" "Failed to parse one or more coordinates." ["x1", m.Groups[2].Value; "y1", m.Groups[3].Value; "x2", m.Groups[4].Value; "y2", m.Groups[5].Value] |> invalidOp
        | _ -> error "match_image_map_item" "Failed to parse image map item index." ["image map item index", m.Groups[1].Value] |> invalidOp
    else None

let match_jump (text : string) (scripts : Script list) : Command_Pre_Parse option =
    let m = jump_regex.Match text
    if m.Success then
        let destination = m.Groups[1].Value
        match scripts |> List.tryFind (fun script -> script.name = destination) with
        | Some script -> script.id |> Jump |> Command_Pre_Parse.Command |> Some
        | None -> error "match_jump" "Jump destination not found." ["destination", destination; "scripts", scripts] |> invalidOp
    else None
