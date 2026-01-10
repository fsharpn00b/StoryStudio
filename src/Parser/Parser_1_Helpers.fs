module Parser_1_Helpers

// Environment.NewLine
open System

open Character_Types
open Command_Types
open Parser_Patterns
open Image_Map
open Log
open Menu
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Parser_1_Helpers"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Helper functions - javascript. These are used to parse all commands that allow javascript interpolation. *)

let extract_javascript_interpolations (input : string) : string list =
    let matches = javascript_interpolation_regex.Matches input
    [ for m in matches -> m.Groups.["interpolation"].Value ]

// TODO2 Consider just letting authors use ${} for JS interpolation. Then we can use {} for other purposes.
let convert_string_to_use_javascript_interpolation (text_1 : string) : string =
    let text_2 = text_1.Replace ("{", "${")
    $"`{text_2}`"

(* Helper functions - error reporting *)

let command_pattern_2_to_signature (command : Command_Pattern_2) : string =
    let signature =
        command.parameters
            |> Seq.map (fun kv ->
                $"({kv.Key} : {kv.Value.ToString()})"
            )
            |> Seq.reduce (fun acc item -> $@"{acc} {item}")

    $"Command: {command.name.ToString ()}. Signature: {command.pattern} {signature}"

(* Helper functions - matched_command_to_pre_parse_command *)

let get_music
    (music_tracks : Map<string, string>)
    (track_name : string)
    : Result<string, Parser_Error> =

    match music_tracks.TryFind track_name with
    | Some url -> Ok url
    | None -> Error ("Unknown music track name.", ["music track name", track_name; "known music tracks", music_tracks])

let get_background
    (backgrounds : Map<string, string>)
    (background_name : string)
    : Result<string, Parser_Error> =

    match backgrounds.TryFind background_name with
    | Some url -> Ok url
    | None -> Error ("Unknown background name.", ["background name", background_name; "known backgrounds", backgrounds])

let get_character
    (characters : Character_Input_Map)
    (character_short_name : string)
    : Result<Character_Input, Parser_Error> =

    match characters.TryFind character_short_name with
    | Some character -> Ok character
    | None -> Error ("Unknown character.", ["character_short_name", character_short_name; "known characters", characters])

let get_character_sprite
    (characters : Character_Input_Map)
    (character_short_name : string)
    (character_sprite_name : string)
    : Result<string, Parser_Error> =

    match get_character characters character_short_name with
    | Ok character ->
        match character.sprites.TryFind character_sprite_name with
        | Some url -> Ok url
        | None -> Error ("Unknown character sprite.", ["sprite", character_sprite_name; "character data", character])
    | Error e -> Error e

let get_script_id
    (scripts : Script list)
    (destination : string)
    : Result<int<scene_id>, Parser_Error> =

    match scripts |> List.tryFind (fun script -> script.name = destination) with
    | Some script -> script.id |> Ok
    | None -> Error ("Jump destination not found.", ["destination", destination; "scripts", scripts])

(* Helper functions - match_commands *)

// TODO1 #parsing Have these collect functions return error lists, rather than failing, if possible.

// TODO1 #parsing Have these report line numbers/lines on failure.

let rec collect_multi_line_comment
    (remaining_commands : (int * string) list)
    : (int * string) list =
    match remaining_commands with
    | [] -> error "collect_multi_line_comment" "Comment block never terminates." [] |> invalidOp
    | (line_number, line) :: tail ->
        if line |> multi_line_comment_end_regex.IsMatch then tail
        else collect_multi_line_comment tail

(* We handle JavaScript blocks here, rather than in pre_nesting_commands_to_post_nesting_commands, because JavaScript statements do not conform to any pattern. When we find a JavaScript start command, we simply record the following commands and add them to the JavaScript block, until we find a JavaScript end command. *)
(* We do not need to discard comments in JavaScript blocks. The browser will handle those when we eval the JavaScript. *)
let rec collect_javascript
    (javascript_acc : string list)
    (remaining_commands : (int * string) list)
    : {|
        command : Command
        remaining_commands : (int * string) list
    |} =
    match remaining_commands with
    | [] -> error "collect_javascript" "JavaScript block never terminates." ["acc", javascript_acc] |> invalidOp
    | (line_number, line) :: tail ->
        if line |> javascript_end_regex.IsMatch then
            match javascript_acc with
            | [] -> error "collect_javascript" "JavaScript block is empty." [] |> invalidOp
            | _ ->
                {|
(* We append each line to the accumulator, so reverse the accumulator before returning it. *)
                    command = javascript_acc |> List.rev |> String.concat Environment.NewLine |> JavaScript_Block                    
                    remaining_commands = tail
                |}
        else collect_javascript (line :: javascript_acc) tail

let match_menu_item (text : string) : Menu_Item_Data_1 option =
    let m = menu_item_regex.Match text
    if m.Success then
        match Int32.TryParse m.Groups["value"].Value with
        | true, value ->
            let text = m.Groups["description"].Value
            {
                value = value
                text = text |> convert_string_to_use_javascript_interpolation
                javascript_interpolations = extract_javascript_interpolations text
(* If the pattern defines an optional group, that group is present in m.Groups even if it was not matched in the input text. *)
                conditional = if m.Groups["conditional"].Success then m.Groups["conditional"].Value |> Some else None
            } |> Some
        | _ -> error "match_menu_item" "Failed to parse menu item index." ["menu item index", m.Groups["value"].Value] |> invalidOp
    else None

let rec collect_menu
    (menu_1_acc : Menu_Data_1)
    (remaining_commands : (int * string) list)
    : {|
        menu_data : Menu_Data_1
        remaining_commands : (int * string) list
    |} =
    match remaining_commands with
    | [] -> error "collect_menu" "Menu block never terminates." ["menu", menu_1_acc] |> invalidOp
    | (line_number, line) :: tail ->
(* Discard single-line comments. *)
        if line |> single_line_comment_regex.IsMatch then
            collect_menu menu_1_acc tail
(* Discard multi-line comments. *)
        elif line |> multi_line_comment_start_regex.IsMatch then
            let remaining_commands = collect_multi_line_comment tail
            collect_menu menu_1_acc remaining_commands
        elif line |> end_menu_regex.IsMatch then
            if 0 = List.length menu_1_acc.items then
                error "collect_menu" "Menu contains no items." ["menu", menu_1_acc] |> invalidOp
            elif menu_1_acc.items |> List.forall (fun item -> item.conditional.IsSome) then
                error "collect_menu" "Menu contains only conditional items. At least one item must be non-conditional." ["menu", menu_1_acc] |> invalidOp
            else {|
(* We append each menu item to the accumulator, so reverse the accumulator before returning it. *)
                menu_data = { menu_1_acc with items = menu_1_acc.items |> List.rev }
                remaining_commands = tail
            |}
        elif line |> menu_item_regex.IsMatch then
            let menu_item = (match_menu_item line).Value
            let menu_2 = { menu_1_acc with items = menu_item :: menu_1_acc.items }
            collect_menu menu_2 tail
        else error "collect_menu" "While parsing menu block, encountered line that is neither menu item nor EndMenu." ["line", line; "menu", menu_1_acc] |> invalidOp

let match_image_map_item (text : string) : Image_Map_Item_Data option =
    let m = image_map_item_regex.Match text
    if m.Success then
        match Int32.TryParse m.Groups["value"].Value with
        | true, value ->
            match
                Int32.TryParse m.Groups["x1"].Value,
                Int32.TryParse m.Groups["y1"].Value,
                Int32.TryParse m.Groups["x2"].Value,
                Int32.TryParse m.Groups["y2"].Value
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
                    conditional = if m.Groups["conditional"].Success then m.Groups["conditional"].Value |> Some else None
                } |> Some
            | _ -> error "match_image_map_item" "Failed to parse one or more coordinates." ["x1", m.Groups["x1"].Value; "y1", m.Groups["y1"].Value; "x2", m.Groups["x2"].Value; "y2", m.Groups["y2"].Value] |> invalidOp
        | _ -> error "match_image_map_item" "Failed to parse image map item index." ["image map item index", m.Groups["value"].Value] |> invalidOp
    else None

let rec collect_image_map
    (image_map_1_acc : Image_Map_Data)
    (remaining_commands : (int * string) list)
    : {|
        image_map_data : Image_Map_Data
        remaining_commands : (int * string) list
    |} =
    match remaining_commands with
    | [] -> error "collect_image_map" "ImageMap block never terminates." ["image_map", image_map_1_acc] |> invalidOp
    | (line_number, line) :: tail ->
(* Discard single-line comments. *)
        if line |> single_line_comment_regex.IsMatch then
            collect_image_map image_map_1_acc tail
(* Discard multi-line comments. *)
        elif line |> multi_line_comment_start_regex.IsMatch then
            let remaining_commands = collect_multi_line_comment tail
            collect_image_map image_map_1_acc remaining_commands
        elif line |> end_image_map_with_parameters_regex.IsMatch then
            if 0 = List.length image_map_1_acc.items then
                error "collect_image_map" "ImageMap contains no items." ["image_map", image_map_1_acc] |> invalidOp
            elif image_map_1_acc.items |> List.forall (fun item -> item.conditional.IsSome) then
                error "collect_image_map" "ImageMap contains only conditional items. At least one item must be non-conditional." ["image_map", image_map_1_acc] |> invalidOp
            else {|
(* We append each image_map item to the accumulator, so reverse the accumulator before returning it. *)
                image_map_data = { image_map_1_acc with items = image_map_1_acc.items |> List.rev }
(* Leave end_image_map in the list of remaining commands. We handle it separately and use it to fade out the image map. *)
                remaining_commands = remaining_commands
            |}
        elif line |> image_map_item_regex.IsMatch then
            let image_map_item = (match_image_map_item line).Value
            let image_map_2 = { image_map_1_acc with items = image_map_item :: image_map_1_acc.items }
            collect_image_map image_map_2 tail
        else error "collect_image_map" "While parsing image_map block, encountered line that is neither image map item nor EndImageMap." ["line", line; "image_map", image_map_1_acc] |> invalidOp

(* TODO1 #parsing Simplify these using the new functions, including match_command_parameters_for_single_overload. That will need to be moved here if we do that.
- Move the menu/etc parameters to the command data block, and call convert_parameters with the pattern for menu, etc. Or just have the menu etc command pattern be a separate block outside the command data array. No need to look it up since this is a special case and we know what the command is.
Or, might be simpler to bypass all that and just use match_image_map_start, etc, as we have them? No, command pattern might reduce boilerplate.
*)

let match_dialogue (text : string) (characters : Character_Input_Map) : Command_Pre_Parse option =
    let m = dialogue_regex.Match text
    if m.Success then
        let character_short_name, text = m.Groups["character"].Value, m.Groups["dialogue"].Value
        match get_character characters character_short_name with
        | Ok character ->
            Dialogue {
                character_short_name = character_short_name
                character_full_name = character.full_name
                text = text |> convert_string_to_use_javascript_interpolation
                javascript_interpolations = text |> extract_javascript_interpolations
            } |> Command_Pre_Parse.Command |> Some
(* Dialogue does not have a command. It simply starts with a character name. Any text after that is the character's dialogue. As a result, dialogue can look like almost any other command. We distinguish it by checking the character name against known character names. Character names also cannot collide with commands. *)
        | Error _ -> None
    else None

let match_menu_start (text : string) : Menu_Data_1 option =
    let m = menu_start_with_parameters_regex.Match text
    if m.Success then
        let description = m.Groups["description"].Value
        {
            name = m.Groups["name"].Value
            description = description |> convert_string_to_use_javascript_interpolation
            items = []
            javascript_interpolations = extract_javascript_interpolations description
        } |> Some
    else None

let match_image_map_start (text : string) (backgrounds : Map<string, string>) : Image_Map_Data option =
    let m = image_map_start_with_parameters_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups["transition_time"].Value with
        | true, transition_time ->
            let background = m.Groups["background"].Value
            match backgrounds.TryFind background with
            | Some url ->
                {
                    name = m.Groups["name"].Value
                    url = url
                    items = []
                    transition_time = transition_time |> LanguagePrimitives.FloatWithMeasure
                } |> Some
            | None -> error "match_image_map_start" "Unknown image map background." ["background", background; "known backgrounds", backgrounds] |> invalidOp
        | _ -> error "match_image_map_start" "Failed to parse transition time." ["transition_time", m.Groups["transition_time"].Value] |> invalidOp
    else None

let match_image_map_end (text : string) : Fade_Transition_Time option =
    let m = end_image_map_with_parameters_regex.Match text
    if m.Success then
        match Double.TryParse m.Groups["transition_time"].Value with
        | true, transition_time -> transition_time |> LanguagePrimitives.FloatWithMeasure |> Some
        | _ -> error "match_image_map_end" "Failed to parse transition time." ["transition_time", m.Groups["transition_time"].Value] |> invalidOp
    else None
