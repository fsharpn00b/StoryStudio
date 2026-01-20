module Parser_1_Semantics

// Double, Int32
open System

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

(* Functions - error checking *)

let private check_menu_items (script_name : string) (menu_name : string) (items_1 : Menu_Item_Data_1 list) : unit =
    let error_data = ["script_name", script_name; "menu_name", menu_name] |> List.map (fun (name, data) -> name, data :> obj)

    if Seq.isEmpty items_1 then
// TODO1 #parsing Report what line was being parsed.
        error "check_menu_items" "Menu must contain at least one menu item." error_data |> invalidOp
    else
        let items_2 = items_1 |> Seq.filter (fun item -> item.conditional.IsNone)
        if Seq.isEmpty items_2 then
            error "check_menu_items" "Menu must contain at least one menu item with no conditional." error_data |> invalidOp

let private check_image_map_items (script_name : string) (image_map_name : string) (items_1 : Image_Map_Item_Data list) : unit =
    let error_data = ["script_name", script_name; "image_map_name", image_map_name] |> List.map (fun (name, data) -> name, data :> obj)

    if Seq.isEmpty items_1 then
        error "check_image_map_items" "Image map must contain at least one image_map item." error_data |> invalidOp
    else
        let items_2 = items_1 |> Seq.filter (fun item -> item.conditional.IsNone)
        if Seq.isEmpty items_2 then
            error "check_image_map_items" "Image map must contain at least one image_map item with no conditional." error_data |> invalidOp

(* Semantics *)

let private get_semantics
    (scripts : Script list)
    (music_tracks : Map<string, string>)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    (script_name : string)
    : obj =

    let semantics = createObj []

(* Automatically applied semantics *)

(* This is the automatically applied version of get_children ().
TODO2 If we use this again, add a null element filter. *)
(* _iter must have a JS rest parameter, which Fable cannot create, so we must emit a JS function. *)
//    semantics?_iter <- emitJsExpr () "(function(...xs) { return xs.map (x => x.ast()); })"

// TODO1 #parsing Where is this used?
(* We think Fable cannot create the this reference. *)
    semantics?_terminal <- emitJsExpr () "(function() { return this.sourceString; })"

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
(* For a non-empty line, extract the statement and ignore leading and trailing whitespace and the newline or end. *)
    semantics?non_empty_line <- fun _ statement _ _ -> statement?ast()
(* Apply the appropriate semantic for this statement. *)
    semantics?statement <- fun statement -> statement?ast()

(* Single-line patterns *)

    semantics?fade_in_background <- fun _ _ background_name _ transition_time ->
        {
            Background_Fade_In_Data.new_url = get_background backgrounds background_name?sourceString
            transition_time = transition_time?ast()
        } |> Background_Fade_In |> Command_Pre_Parse.Command
    semantics?fade_out_background <- fun _ _ transition_time ->
        {
            Background_Fade_Out_Data.transition_time = transition_time?ast()
        } |> Background_Fade_Out |> Command_Pre_Parse.Command
    semantics?cross_fade_background <- fun _ _ background_name _ transition_time ->
        {
            Background_Cross_Fade_Data.new_url = get_background backgrounds background_name?sourceString
            transition_time = transition_time?ast()
        } |> Background_Cross_Fade |> Command_Pre_Parse.Command
    semantics?fade_in_character <- fun _ _ character_short_name _ character_sprite_name _ position _ transition_time ->
        {
            Character_Fade_In_Data.character_short_name = character_short_name?sourceString
            url = get_character_sprite characters character_short_name?sourceString character_sprite_name?sourceString
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
            url = get_character_sprite characters character_short_name?sourceString character_sprite_name?sourceString
            transition_time = transition_time?ast()
        } |> Character_Cross_Fade  |> Command_Pre_Parse.Command
    semantics?fade_out_all <- fun _ _ transition_time ->
        transition_time?ast() |> Fade_Out_All |> Command_Pre_Parse.Command
    semantics?play_music <- fun _ _ music_track_name ->
        music_track_name?sourceString |> get_music music_tracks |> Music_Play |> Command_Pre_Parse.Command
    semantics?stop_music <- fun _ -> Music_Stop |> Command_Pre_Parse.Command
    semantics?show_dialogue_box <- fun _ -> Dialogue_Box_Show |> Command_Pre_Parse.Command
    semantics?hide_dialogue_box <- fun _ -> Dialogue_Box_Hide |> Command_Pre_Parse.Command
    semantics?single_line_javascript <- fun _ _ code -> code?sourceString |> JavaScript_Inline |> Command_Pre_Parse.Command
    semantics?``if`` <- fun _ _ conditional -> conditional?sourceString |> Command_Pre_Parse.If
    semantics?else_if <- fun _ _ conditional -> conditional?sourceString |> Command_Pre_Parse.Else_If
    semantics?``else`` <- fun _ -> Command_Pre_Parse.Else
    semantics?end_if <- fun _ -> Command_Pre_Parse.End_If
    semantics?jump <- fun _ _ destination ->
        destination?sourceString |> get_script_id scripts |> Command.Jump |> Command_Pre_Parse.Command
    semantics?hide_image_map <- fun _ _ transition_time ->
        transition_time?ast() |> Command_Pre_Parse.End_Image_Map

    semantics?dialogue <- fun character_short_name _ text ->
        {
            Dialogue_Data.character_short_name = character_short_name?sourceString
            character_full_name =
                let character = get_character characters character_short_name?sourceString
                character.full_name
            text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
        } |> Command.Dialogue |> Command_Pre_Parse.Command

(* Multi-line patterns *)

(* Ignore all comments. *)
    semantics?comment <- fun _ -> null

    semantics?multi_line_javascript <- fun _ code _ ->
        code?sourceString |> JavaScript_Block |> Command_Pre_Parse.Command

(* Notes
- menu_item_conditional? is an optional parameter to menu_item.
- If menu_item_conditional? is not present, this semantic is never applied.
- This semantic is not applied until the menu_item semantic calls menu_item_conditional?ast ().
(end)
Therefore, this is not the place to see whether menu_item_conditional is present or not, nor to convert it to Some/None. The only purpose of this semantic is to extract the conditional clause itself and discard the excess parameters.
*)
    semantics?menu_item_conditional <- fun _ _ _ conditional -> conditional?sourceString
    semantics?menu_item <- fun value _ text conditional_1 ->
        {
            Menu_Item_Data_1.value = value?ast()
            text = text?sourceString |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = extract_javascript_interpolations text?sourceString
            conditional =
// TODO1 #parsing It seems using ?ast() overrides type checking on the result. We can assign the result to a value or field of any type, including to an option field without using Some or None.
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
                let menu_items_2 =
                    menu_items_1?ast()
                        |> unbox<Menu_Item_Data_1 array>
                        |> Array.toList
                do check_menu_items script_name name menu_items_2
                menu_items_2

        } |> Command_Pre_Parse.Menu

    semantics?image_map_item_conditional <- fun _ _ _ conditional -> conditional?sourceString
    semantics?image_map_item <- fun value _ x1 _ y1 _ x2 _ y2 conditional_1 ->
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
    semantics?image_map <- fun _ _ name _ background _ transition_time _ image_map_items_1 _ ->
        {
            Image_Map_Data.name = name?sourceString
            url = get_background backgrounds background?sourceString
            items =
                let image_map_items_2 =
                    image_map_items_1?ast()
                        |> unbox<Image_Map_Item_Data array>
                        |> Array.toList
                do check_image_map_items script_name name image_map_items_2
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

    semantics

// TODO1 #parsing How can we get clearer error messages from the parser?

let private parse_script_2
    (scripts : Script list)
    (music_tracks : Map<string, string>)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    (script_name : string)
    (script_text : string)
    : Command_Pre_Parse list =

    let grammar_text = get_grammar_text characters
    let grammar : obj = ohm?grammar(grammar_text)
    let grammar_match_result : obj = grammar?``match``(script_text)

    if grammar_match_result?failed() then
        error "parse_ohm" grammar_match_result?message [] |> invalidOp

    let semantics_1 = get_semantics scripts music_tracks backgrounds characters script_name
    let semantics_2 : obj =
        grammar?createSemantics()?addOperation("ast", semantics_1)
    let semantics_3 : obj =
        emitJsExpr (semantics_2, grammar_match_result) "$0($1)"

    let semantics_application_result =
        semantics_3?ast()
            |> unbox<obj array>
            |> Array.choose (fun (item : obj) ->
                if isNull item then None
                else item :?> Command_Pre_Parse |> Some
            )
            |> Array.toList

    #if debug
    debug "parse_ohm" String.Empty ["semantics_application_result", json_stringify semantics_application_result]
    #endif

    semantics_application_result

let parse_script_1
    (scripts : Script list)
    (music_tracks : Map<string, string>)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    (script_name : string)
    (script_text : string)
    : Command_Pre_Parse list =

    try parse_script_2 scripts music_tracks backgrounds characters script_name script_text
    with exn -> error "parse_script_1" exn.Message ["script_name", script_name] |> invalidOp
