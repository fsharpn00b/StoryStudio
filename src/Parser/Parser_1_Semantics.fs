module Parser_1_Semantics

// Double, Int32
open System
// Regex
open System.Text.RegularExpressions

// console, window
open Browser.Dom
// ? operator, createObj, emitJsExpr, importAll
open Fable.Core.JsInterop

(* To install, run
npm install ohm-js
(end)
*)
let ohm : obj = importAll "ohm-js"

open Background
open Character_Types
open Command_Types
open Image_Map
open Log
open Menu
open Temporary_Notification
open Parser_1_Helpers
open Parser_1_Grammar
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Parser_Semantics"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Functions - helper *)

let private get_children<'T> (node : obj) : 'T list =
    node?children
        |> Seq.map (fun c -> c?ast() |> unbox<'T option>)
(* Semantics determine which elements are None, so we need to call ast () to apply the semantics to each element first. *)
        |> Seq.choose id
        |> Seq.toList

(* Semantics *)

// TODO2 #parsing It seems using ?ast() overrides type checking on the result. We can assign the result to a value or field of any type, including to an option field without using Some or None. See if we can get better type safety here. We have somewhat done this as of 20260202.

let private get_semantics
    (scripts : Script list)
    (music_tracks : Map<string, string>)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
(* Note This does not return a Command_Pre_Parse list. It returns a semantics object, on which we call ast (). That returns the Command_Pre_Parse list. *)
    : obj =

    let semantics = createObj []

(* Automatically applied semantics *)

(* This is the automatically applied version of get_children ().
TODO2 If we use this again, add a null/None element filter. *)
(* _iter must have a JS rest parameter, which Fable cannot create, so we must emit a JS function. *)
//    semantics?_iter <- emitJsExpr () "(function(...xs) { return xs.map (x => x.ast()); })"

(* We think this is called automatically if a semantic handler simply returns an object instead of returning the value of its sourceString property or the result of calling ast() on it.
We do not currently use this. *)
(* We think Fable cannot create the this reference. *)
//    semantics?_terminal <- emitJsExpr () "(function() { return this.sourceString; })"

(* Miscellaneous patterns *)

    semantics?float_param <- fun before _ after ->
        let before = before?sourceString
        let after = after?sourceString
        if String.IsNullOrEmpty before && String.IsNullOrEmpty after then 0.0
        elif String.IsNullOrEmpty before then Double.Parse $"0.{after}"
        elif String.IsNullOrEmpty after then Double.Parse before
        else Double.Parse $"{before}.{after}"

    semantics?int_param <- fun i -> Int32.Parse i?sourceString

(* Top level patterns *)

    semantics?script <- fun lines _ -> get_children<Command_Pre_Parse_Type> lines

    semantics?line <- fun line -> line?ast()
(* Ignore empty lines. *)
    semantics?empty_line <- fun _ _ -> None
(* For a non-empty line, extract the statement, menu, or image map, and ignore leading and trailing whitespace and the newline or end. *)
    semantics?non_empty_line <- fun _ line _ _ _ ->
        match line?ast() |> unbox<Command_Pre_Parse_Type option> with
        | None -> None
        | Some command ->
            {
                Command_Pre_Parse_1.error_data = {
                    source = line?sourceString
                    script_text_index = line?source?startIdx
                }
                command = command
            } |> Some

(* Apply the appropriate semantic for this statement. *)
    semantics?statement <- fun statement -> statement?ast()

(* Single-line patterns *)

    semantics?fade_in_background <- fun _ _ background_name _ transition_time ->
        {
            Background_Fade_In_Data.new_url = get_background_url backgrounds background_name?sourceString background_name?source?startIdx
            transition_time = transition_time?ast()
        } |> Background_Fade_In |> Command_Pre_Parse_Type.Command |> Some
    semantics?fade_out_background <- fun _ _ transition_time ->
        {
            Background_Fade_Out_Data.transition_time = transition_time?ast()
        } |> Background_Fade_Out |> Command_Pre_Parse_Type.Command |> Some
    semantics?cross_fade_background <- fun _ _ background_name _ transition_time ->
        {
            Background_Cross_Fade_Data.new_url = get_background_url backgrounds background_name?sourceString background_name?source?startIdx
            transition_time = transition_time?ast()
        } |> Background_Cross_Fade |> Command_Pre_Parse_Type.Command |> Some
    semantics?fade_in_character <- fun _ _ character_short_name _ character_sprite_name _ position _ transition_time ->
        {
            Character_Fade_In_Data.character_short_name = character_short_name?sourceString
            url = get_character_sprite_url characters character_short_name?sourceString character_sprite_name?sourceString character_short_name?source?startIdx
            position = position?ast()
            transition_time = transition_time?ast()
        } |> Character_Fade_In |> Command_Pre_Parse_Type.Command |> Some
    semantics?fade_out_character <- fun _ _ character_short_name _ transition_time->
        {
            Character_Fade_Out_Data.character_short_name = character_short_name?sourceString
            transition_time = transition_time?ast()
        } |> Character_Fade_Out |> Command_Pre_Parse_Type.Command |> Some
    semantics?cross_fade_character <- fun _ _ character_short_name _ character_sprite_name _ transition_time ->
        {
            Character_Cross_Fade_Data.character_short_name = character_short_name?sourceString
            url = get_character_sprite_url characters character_short_name?sourceString character_sprite_name?sourceString character_short_name?source?startIdx
            transition_time = transition_time?ast()
        } |> Character_Cross_Fade  |> Command_Pre_Parse_Type.Command |> Some
    semantics?fade_out_all <- fun _ _ transition_time ->
        transition_time?ast() |> Fade_Out_All |> Command_Pre_Parse_Type.Command |> Some
    semantics?move_in_character <- fun _ _ character_short_name _ character_sprite_name _ direction _ position _ transition_time ->
        get_move_in_semantics characters character_short_name?sourceString character_sprite_name?sourceString direction?sourceString (position?ast()) (transition_time?ast()) character_short_name?source?startIdx |> Some
    semantics?move_out_character <- fun _ _ character_short_name _ direction _ transition_time ->
        get_move_out_semantics character_short_name?sourceString direction?sourceString (transition_time?ast()) character_short_name?source?startIdx |> Some
    semantics?play_music <- fun _ _ music_track_name ->
        let music_track_url = get_music_track_url music_tracks music_track_name?sourceString music_track_name?source?startIdx
        music_track_url |> Music_Play |> Command_Pre_Parse_Type.Command |> Some
    semantics?stop_music <- fun _ -> Music_Stop |> Command_Pre_Parse_Type.Command |> Some
    semantics?show_dialogue_box <- fun _ -> Dialogue_Box_Show |> Command_Pre_Parse_Type.Command |> Some
    semantics?hide_dialogue_box <- fun _ -> Dialogue_Box_Hide |> Command_Pre_Parse_Type.Command |> Some
    semantics?single_line_javascript <- fun _ _ code ->
        {
            code = code?sourceString
            script_text_index = code?source?startIdx
        } |> JavaScript_Inline |> Command_Pre_Parse_Type.Command |> Some
    semantics?``if`` <- fun _ _ conditional -> conditional?sourceString |> Command_Pre_Parse_Type.If |> Some
    semantics?else_if <- fun _ _ conditional -> conditional?sourceString |> Command_Pre_Parse_Type.Else_If |> Some
    semantics?``else`` <- fun _ -> Command_Pre_Parse_Type.Else |> Some
    semantics?end_if <- fun _ -> Command_Pre_Parse_Type.End_If |> Some
    semantics?jump <- fun _ _ destination ->
        let script_id = get_script_id scripts destination?sourceString destination?source?startIdx
        script_id |> Command_Type.Jump |> Command_Pre_Parse_Type.Command |> Some
    semantics?hide_image_map <- fun _ _ transition_time ->
        transition_time?ast() |> Command_Pre_Parse_Type.End_Image_Map |> Some

    semantics?dialogue <- fun character_short_name _ text ->
        {
            Dialogue_Data.character_short_name = character_short_name?sourceString
            character_full_name =
                let character = get_character_input_data characters character_short_name?sourceString character_short_name?source?startIdx
                character.full_name
            text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
        } |> Command_Type.Dialogue |> Command_Pre_Parse_Type.Command |> Some

(* Multi-line patterns *)

(* Ignore all comments. *)
    semantics?comment <- fun _ -> None

    semantics?multi_line_javascript <- fun _ code_1 _ ->
        let code_2 = code_1?sourceString |> unbox<string>
        {
(* Remove leading and trailing whitespace from the code block. *)
            code = code_2.Trim ()
            script_text_index = code_1?source?startIdx
        } |> JavaScript_Block |> Command_Pre_Parse_Type.Command |> Some

(* Note It seems if a menu item has no conditional, its children property is empty, and this semantic is never applied. *)
    semantics?menu_item_conditional <- fun _ _ _ conditional -> Some conditional?sourceString
    semantics?menu_item <- fun value _ text conditional_1 _ ->
        {
            Menu_Item_Data_1.value = value?ast()
            text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
            conditional =
(* Note It seems zero or one (?) is treated as an iterable. We cannot simply call ast () on it. *)
                let conditional_2 = get_children<string> conditional_1
                match conditional_2 with
                | head :: _ -> Some head
                | [] -> None
        } |> Some
    semantics?menu_empty_line <- fun _ -> None
(* Convert the menu items to a sequence, convert comments and empty lines to None, and remove them. *)
    semantics?menu_items <- fun items _ -> get_children<Menu_Item_Data_1> items
    semantics?menu <- fun _ _ name _ text _ menu_items_1 _ ->
        {
            Menu_Data_1.name = name?sourceString
            text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
            items =
(* We call get_children in semantics?menu_items, which we call with menu_items_1?ast(). *)
                let menu_items_2 = menu_items_1?ast()
                do check_menu_items name?sourceString menu_items_2 menu_items_1?source?startIdx
                menu_items_2

        } |> Command_Pre_Parse_Type.Menu |> Some

(* Note It seems if an image map item has no conditional, its children property is empty, and this semantic is never applied. *)
    semantics?image_map_item_conditional <- fun _ _ _ conditional -> Some conditional?sourceString
    semantics?image_map_item <- fun value _ x1 _ y1 _ x2 _ y2 conditional_1 _ ->
        {
            Image_Map_Item_Data.value = value?ast()
            x1 = x1?ast()
            y1 = y1?ast()
            x2 = x2?ast()
            y2 = y2?ast()
// TODO2 This is for future use with alt text.
            javascript_interpolations = []
            conditional =
(* Note It seems zero or one (?) is treated as an iterable. We cannot simply call ast () on it. *)
                let conditional_2 = get_children<string> conditional_1
                match conditional_2 with
                | head :: _ -> Some head
                | [] -> None
        }
    semantics?image_map_empty_line <- fun _ -> None
(* Convert the image map items to a sequence, convert comments and empty lines to None, and remove them. *)
    semantics?image_map_items <- fun items _ -> get_children<Image_Map_Item_Data> items
    semantics?image_map <- fun _ _ name _ background_name _ transition_time _ image_map_items_1 _ ->
        {
            Image_Map_Data.name = name?sourceString
            url = get_background_url backgrounds background_name?sourceString background_name?source?startIdx
            items =
(* We call get_children in semantics?image_map_items, which we call with image_map_items_1?ast(). *)
                let image_map_items_2 = image_map_items_1?ast()
                do check_image_map_items name?sourceString image_map_items_2 image_map_items_1?source?startIdx
                image_map_items_2
            transition_time = transition_time?ast()
        } |> Command_Pre_Parse_Type.Image_Map |> Some

    semantics?temporary_notification <- fun _ _ text _ ->
        {
            Notification_Data_1.text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
        } |> Temporary_Notification |> Command_Pre_Parse_Type.Command |> Some
    semantics?permanent_notification <- fun _ _ text _ ->
        {
            Notification_Data_1.text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
        } |> Permanent_Notification |> Command_Pre_Parse_Type.Command |> Some

    semantics

// TODO1 #parsing How can we get clearer error messages from the parser?

let get_grammar_and_semantics
    (scripts : Script list)
    (music_tracks : Map<string, string>)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    : obj * obj =

    let grammar_text = get_grammar_text characters
    let grammar = ohm?grammar(grammar_text)

    let semantics_1 = get_semantics scripts music_tracks backgrounds characters
    let semantics_2 = grammar?createSemantics()?addOperation("ast", semantics_1)

    grammar, semantics_2

let private parse_script_2
    (grammar : obj)
    (semantics_1 : obj)
    (script : Script)
    : Command_Pre_Parse_2 list =

    let grammar_match_result : obj = grammar?``match``(script.content)

    if grammar_match_result?failed() then
        error "parse_script_2" "Failed to parse script." ["script_name", script.name; "message", grammar_match_result?message] |> invalidOp

    let semantics_2 : obj =
        emitJsExpr (semantics_1, grammar_match_result) "$0($1)"

    let semantics_application_result = semantics_2?ast() |> unbox<Command_Pre_Parse_1 list>

    #if debug
    debug "parse_script_2" String.Empty ["semantics_application_result", json_stringify semantics_application_result]
    #endif

(* Add the scene ID to each command. *)
    semantics_application_result
        |> List.map (fun command ->
            {
                error_data = {
                    source = command.error_data.source
                    scene_id = script.id
                    script_text_index = command.error_data.script_text_index
                }
                command = command.command
            })

let parse_script_1
    (grammar : obj)
    (semantics : obj)
    (script : Script)
    : Command_Pre_Parse_2 list =

    try parse_script_2 grammar semantics script with
    | Parsing_Semantics_Error e ->
        let script_line_number = get_script_line_number script.content e.script_text_index
        error "parse_script_1" "Failed to parse script." (e.data @ [
            "message", e.message
            "script_name", script.name
            "script_line_number", script_line_number
        ]) |> invalidOp
(* We should not see a generic exception here, so we just re-raise this. *)
    | _ -> reraise ()
