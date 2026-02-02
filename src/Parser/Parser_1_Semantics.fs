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
open Utilities

(* Debug *)

let debug_module_name = "Parser_Semantics"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Functions - helper *)

let private get_children (node : obj) : obj array =
    node?children
        |> Seq.map (fun c -> c?ast())
(* Semantics determine which elements are null, so we need to call ast () to apply the semantics to each element first. *)
        |> Seq.filter (fun x -> not (isNull x))
        |> Seq.toArray

(* Semantics *)

let private get_semantics
    (scripts : Script list)
    (music_tracks : Map<string, string>)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    : obj =

    let semantics = createObj []

(* Automatically applied semantics *)

(* This is the automatically applied version of get_children ().
TODO2 If we use this again, add a null element filter. *)
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

    semantics?script <- fun lines _ -> get_children lines
    semantics?line <- fun x -> x?ast()
(* Ignore empty lines. *)
    semantics?empty_line <- fun _ _ -> null
(* For a non-empty line, extract the statement, menu, or image map, and ignore leading and trailing whitespace and the newline or end. *)
    semantics?non_empty_line <- fun _ x _ _ _ -> x?ast()
(* Apply the appropriate semantic for this statement. *)
    semantics?statement <- fun statement -> statement?ast()

(* Single-line patterns *)

    semantics?fade_in_background <- fun _ _ background_name _ transition_time ->
        {
            Background_Fade_In_Data.new_url = get_background_url backgrounds background_name?sourceString background_name?source?startIdx
            transition_time = transition_time?ast()
        } |> Background_Fade_In |> Command_Pre_Parse.Command
    semantics?fade_out_background <- fun _ _ transition_time ->
        {
            Background_Fade_Out_Data.transition_time = transition_time?ast()
        } |> Background_Fade_Out |> Command_Pre_Parse.Command
    semantics?cross_fade_background <- fun _ _ background_name _ transition_time ->
        {
            Background_Cross_Fade_Data.new_url = get_background_url backgrounds background_name?sourceString background_name?source?startIdx
            transition_time = transition_time?ast()
        } |> Background_Cross_Fade |> Command_Pre_Parse.Command
    semantics?fade_in_character <- fun _ _ character_short_name _ character_sprite_name _ position _ transition_time ->
        {
            Character_Fade_In_Data.character_short_name = character_short_name?sourceString
            url = get_character_sprite_url characters character_short_name?sourceString character_sprite_name?sourceString character_short_name?source?startIdx
            position = position?ast()
            transition_time = transition_time?ast()
        } |> Character_Fade_In |> Command_Pre_Parse.Command
    semantics?fade_out_character <- fun _ _ character_short_name _ transition_time->
        {
            Character_Fade_Out_Data.character_short_name = character_short_name?sourceString
            transition_time = transition_time?ast()
        } |> Character_Fade_Out |> Command_Pre_Parse.Command
    semantics?cross_fade_character <- fun _ _ character_short_name _ character_sprite_name _ transition_time ->
        {
            Character_Cross_Fade_Data.character_short_name = character_short_name?sourceString
            url = get_character_sprite_url characters character_short_name?sourceString character_sprite_name?sourceString character_short_name?source?startIdx
            transition_time = transition_time?ast()
        } |> Character_Cross_Fade  |> Command_Pre_Parse.Command
    semantics?fade_out_all <- fun _ _ transition_time ->
        transition_time?ast() |> Fade_Out_All |> Command_Pre_Parse.Command
    semantics?move_in_character <- fun _ _ character_short_name _ character_sprite_name _ direction _ position _ transition_time ->
        get_move_in_semantics characters character_short_name?sourceString character_sprite_name?sourceString direction?sourceString (position?ast()) (transition_time?ast()) character_short_name?source?startIdx
    semantics?move_out_character <- fun _ _ character_short_name _ direction _ transition_time ->
        get_move_out_semantics character_short_name?sourceString direction?sourceString (transition_time?ast()) character_short_name?source?startIdx
    semantics?play_music <- fun _ _ music_track_name ->
        let music_track_url = get_music_track_url music_tracks music_track_name?sourceString music_track_name?source?startIdx
        music_track_url |> Music_Play |> Command_Pre_Parse.Command
    semantics?stop_music <- fun _ -> Music_Stop |> Command_Pre_Parse.Command
    semantics?show_dialogue_box <- fun _ -> Dialogue_Box_Show |> Command_Pre_Parse.Command
    semantics?hide_dialogue_box <- fun _ -> Dialogue_Box_Hide |> Command_Pre_Parse.Command
    semantics?single_line_javascript <- fun _ _ code ->
        {
            code = code?sourceString
            script_text_index = code?source?startIdx
        } |> JavaScript_Inline |> Command_Pre_Parse.Command
    semantics?``if`` <- fun _ _ conditional -> conditional?sourceString |> Command_Pre_Parse.If
    semantics?else_if <- fun _ _ conditional -> conditional?sourceString |> Command_Pre_Parse.Else_If
    semantics?``else`` <- fun _ -> Command_Pre_Parse.Else
    semantics?end_if <- fun _ -> Command_Pre_Parse.End_If
    semantics?jump <- fun _ _ destination ->
        let script_id = get_script_id scripts destination?sourceString destination?source?startIdx
        script_id |> Command.Jump |> Command_Pre_Parse.Command
    semantics?hide_image_map <- fun _ _ transition_time ->
        transition_time?ast() |> Command_Pre_Parse.End_Image_Map

    semantics?dialogue <- fun character_short_name _ text ->
        {
            Dialogue_Data.character_short_name = character_short_name?sourceString
            character_full_name =
                let character = get_character_input_data characters character_short_name?sourceString character_short_name?source?startIdx
                character.full_name
            text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
        } |> Command.Dialogue |> Command_Pre_Parse.Command

(* TODO1 #parsing We want the author to be able to use js for arbitrary commands, for example fadein ${background_name} ${fade_in_time}. That would be cumbersome and ugly, even under the old line-by-line parser, because the variables can't be replaced by values until runtime.

We could add a dynamic eval command.
1 Grammar: eval/endeval
2 In semantics, apply convert_string_to_use_javascript_interpolation.
3 In runner, run eval_js. That returns a new command.
4 Apply grammar to command.
5 Apply semantics to grammar match.
6 Run resulting command.

- Problem, resulting command won't have a command ID. We could assign it the next available, in case it for example starts a transition. It can't be an If or other control statement.
*)

(* Multi-line patterns *)

(* Ignore all comments. *)
    semantics?comment <- fun _ -> null

    semantics?multi_line_javascript <- fun _ code_1 _ ->
        let code_2 = code_1?sourceString |> unbox<string>
        {
(* Remove leading and trailing whitespace from the code block. *)
            code = code_2.Trim ()
            script_text_index = code_1?source?startIdx
        } |> JavaScript_Block |> Command_Pre_Parse.Command

(* Notes
- menu_item_conditional? is an optional parameter to menu_item.
- If menu_item_conditional? is not present, this semantic is never applied.
- This semantic is not applied until the menu_item semantic calls menu_item_conditional?ast ().
(end)
Therefore, this is not the place to see whether menu_item_conditional is present or not, nor to convert it to Some/None. The only purpose of this semantic is to extract the conditional clause itself and discard the excess parameters.
*)
    semantics?menu_item_conditional <- fun _ _ _ conditional -> conditional?sourceString
    semantics?menu_item <- fun value _ text conditional_1 _ ->
        {
            Menu_Item_Data_1.value = value?ast()
            text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
            conditional =
// TODO1 #parsing It seems using ?ast() overrides type checking on the result. We can assign the result to a value or field of any type, including to an option field without using Some or None. See if we can get better type safety here.
(* If menu_item_conditional? is not matched, the sourceString of the corresponding parameter is empty. *)
                if String.IsNullOrEmpty conditional_1?sourceString then None
(* Note It seems zero or one (?) is treated as an iterable. *)
                else
                    let conditional_2 = get_children conditional_1
                    Some (string <| conditional_2[0])
        }
    semantics?menu_empty_line <- fun _ -> null
(* Convert the menu items to a sequence, convert comments to null, and remove them. *)
    semantics?menu_items <- fun items _ -> get_children items
    semantics?menu <- fun _ _ name _ text _ menu_items_1 _ ->
        {
            Menu_Data_1.name = name?sourceString
            text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
            items =
(* We call get_children in semantics?menu_items, which we call with menu_items_1?ast(). However, get_children returns an obj array, so we must still convert it to a Menu_Item_Data_1 array with unbox. *)
                let menu_items_2 =
                    menu_items_1?ast()
                        |> unbox<Menu_Item_Data_1 array>
                        |> Array.toList
                do check_menu_items name?sourceString menu_items_2 menu_items_1?source?startIdx
                menu_items_2

        } |> Command_Pre_Parse.Menu

    semantics?image_map_item_conditional <- fun _ _ _ conditional -> conditional?sourceString
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
(* If image_map_item_conditional? is not matched, the sourceString of the corresponding parameter is empty. *)
                if String.IsNullOrEmpty conditional_1?sourceString then None
(* Note It seems zero or one (?) is treated as an iterable. *)
                else
                    let conditional_2 = get_children conditional_1
                    Some (string <| conditional_2[0])
        }
    semantics?image_map_empty_line <- fun _ -> null
(* Convert the image map items to a sequence, convert comments to null, and remove them. *)
    semantics?image_map_items <- fun items _ -> get_children items
    semantics?image_map <- fun _ _ name _ background_name _ transition_time _ image_map_items_1 _ ->
        {
            Image_Map_Data.name = name?sourceString
            url = get_background_url backgrounds background_name?sourceString background_name?source?startIdx
            items =
(* We call get_children in semantics?image_map_items, which we call with image_map_items_1?ast(). However, get_children returns an obj array, so we must still convert it to a Image_Map_Item_Data array with unbox. *)
                let image_map_items_2 =
                    image_map_items_1?ast()
                        |> unbox<Image_Map_Item_Data array>
                        |> Array.toList
                do check_image_map_items name?sourceString image_map_items_2 image_map_items_1?source?startIdx
                image_map_items_2
            transition_time = transition_time?ast()
        } |> Command_Pre_Parse.Image_Map

    semantics?temporary_notification <- fun _ _ text _ ->
        {
            Notification_Data_1.text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
        } |> Temporary_Notification |> Command_Pre_Parse.Command
    semantics?permanent_notification <- fun _ _ text _ ->
        {
            Notification_Data_1.text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
        } |> Permanent_Notification |> Command_Pre_Parse.Command

(* TODO1 #javascript 20260202
- Find a chokepoint in get_semantics () where, for any given command, we can also record the sourceString and source?startIdx.
- When we run a command, find a chokepoint where we can catch exceptions and get the line number for the command in question.

- When checking JS, emit the source string and line number for each command.

- Throw exceptions normally, adding the data array to the Exception.Data field. Then handle all exceptions at the top level. Show the window.alert there.
*)

    semantics

// TODO1 #parsing How can we get clearer error messages from the parser?

let get_grammar_and_semantics
    (scripts : Script list)
    (music_tracks : Map<string, string>)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    : obj * obj =

    try
        let grammar_text = get_grammar_text characters
        let grammar = ohm?grammar(grammar_text)

        let semantics_1 = get_semantics scripts music_tracks backgrounds characters
        let semantics_2 = grammar?createSemantics()?addOperation("ast", semantics_1)

        grammar, semantics_2
(* This should not happen, but get_semantics uses the ? operator, which prevents type checking, so we handle exceptions to be safe. *)
    with exn -> error "get_grammar_and_semantics" exn.Message ["exception", exn] |> invalidOp

let private parse_script_2
    (grammar : obj)
    (semantics_1 : obj)
    (script_text : string)
    : Command_Pre_Parse list =

    let grammar_match_result : obj = grammar?``match``(script_text)

    if grammar_match_result?failed() then
        raise <| Exception grammar_match_result?message

    let semantics_2 : obj =
        emitJsExpr (semantics_1, grammar_match_result) "$0($1)"

(* TODO1 #javascript To add script name and line number to JavaScript commands for error reporting, we would need to transform them here. This is the simplest place to do it, as we have the script text and can get the script name. The down side is we must split the JavaScript_Data and maybe Command and Command_Pre_Parse with it. Or we could add option fields to JavaScript_Data. This is simplest, at the cost of allowing a potentially invalid state. Given these fields will only be used for error reporting and can be checked first for safety, that might be acceptable.

Or, when we hit a JS exception, look up the script name and text in Runner_Queue_Helpers.handle_command () using the scene id and scene map? We opted for that since we can also use that information when checking JS.
*)

    let semantics_application_result =
        semantics_2?ast()
            |> unbox<obj array>
            |> Array.choose (fun (item : obj) ->
                if isNull item then None
                else item :?> Command_Pre_Parse |> Some
            )
            |> Array.toList

    #if debug
    debug "parse_script_2" String.Empty ["semantics_application_result", json_stringify semantics_application_result]
    #endif

    semantics_application_result

let parse_script_1
    (grammar : obj)
    (semantics : obj)
    (script_name : string)
    (script_text : string)
    : Command_Pre_Parse list =

    try parse_script_2 grammar semantics script_text
    with
        | Semantic_Error exn ->
            let line_number = get_script_line_number script_text exn.script_text_index
            error "parse_script_1" exn.message (exn.data @ ["script_name", script_name; "line_number", line_number]) |> invalidOp
        | exn -> error "parse_script_1" exn.Message ["script_name", script_name] |> invalidOp
