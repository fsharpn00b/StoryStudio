module Parser_1_Grammar

// String
open System

// console, window
open Browser.Dom

open Character_Types
open Log
open Utilities

(* Debug *)

let debug_module_name = "Parser_Grammar"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Grammar *)

(* Note A rule must start with a lower-case letter, or it will allow whitespace. For example, "Int_Param = digit+" would match "1 2".
See
https://ohmjs.org/docs/syntax-reference#syntactic-lexical
*)
let get_grammar_text (characters : Character_Input_Map) : string =

(* Dialogue does not have a command. It simply starts with a character name. Any text after that is the character's dialogue. As a result, dialogue can look like almost any other command. We distinguish it by checking the character name against known character names. Character names also cannot collide with commands. *)
    let character_short_names_1 =
        characters
        |> Seq.map (fun kv -> $"\"{kv.Key}\"")
    let character_short_names_2 = String.Join (" | ", character_short_names_1)
    let dialogue_pattern = $"dialogue = ({ character_short_names_2 }) sp+ (~nl any)+"

    let grammar_text =
        """
Script {
/* Note Unlike with code, it seems we must define top-level patterns before the lower-level patterns they comprise. */
/* Top-level patterns. */
    script = line* end

    line =
        empty_line
        | non_empty_line
    empty_line = sp* nl
/* TODO2 #parsing Consider just using regex to remove empty lines, comments, and whitespace at the start and end of each line from the script before parsing it with ohm.js. That would make the grammar simpler and less error-prone.
*/
    non_empty_line =
        sp* statement sp* single_line_comment? nl
        | sp* statement sp* single_line_comment? end
/* Menu and image map require the player to make a choice, so they should not go at the end of a script. */
        | sp* menu sp* single_line_comment? nl
        | sp* image_map sp* single_line_comment? nl

/* Miscellaneous patterns. */
    string_param = (alnum | "_" | "-") +
    int_param = digit+
    float_param = digit* "." digit*

    nl = "\n" | "\r\n"
    sp = " "

    statement =
/* Single-line patterns. */
        | fade_in_background
        | fade_out_background
        | cross_fade_background
        | fade_in_character
        | fade_out_character
        | cross_fade_character
        | fade_out_all
        | move_in_character
        | move_out_character
        | play_music
        | stop_music
        | show_dialogue_box
        | hide_dialogue_box
        | hide_permanent_notification
        | single_line_javascript
        | if
        | else_if
        | else
        | end_if
        | jump
/* TODO2 We had to separate end_image_map (which terminates the image_map_item list) and hide_image_map (which contains the transition time). The latter requires a separate command. We could not find a way to make the image_map rule work without having the image_map semantic also consume end_image_map. We tried a lookahead but that did not work. This means the image_map semantic would have to return two commands, image_map and end_image_map (with a transition time). One workaround would be to have each semantic return a list of commands, so that its ast() function would return an obj array array, then call Array.concat () on the result.
*/
        | hide_image_map
        | dialogue
        | eval

/* Multi-line patterns. */
        | comment
        | multi_line_javascript
        | temporary_notification
        | permanent_notification

/* Single-line patterns. */

// TODO2 #parsing Allow aliases for these commands.
    fade_in_background = "fadein" sp+ string_param sp+ float_param
    fade_out_background = "fadeout" sp+ float_param
    cross_fade_background = "fadeto" sp+ string_param sp+ float_param
    fade_in_character = "fadein" sp+ string_param sp+ string_param sp+ int_param sp+ float_param
    fade_out_character = "fadeout" sp+ string_param sp+ float_param
    cross_fade_character = "fadeto" sp+ string_param sp+ string_param sp+ float_param 
    fade_out_all = "fadeoutall" sp+ float_param
    move_in_character = "movein" sp+ string_param sp+ string_param sp+ ("left" | "right") sp+ int_param sp+ float_param
    move_out_character = "moveout" sp+ string_param sp+ ("left" | "right") sp+ float_param
    play_music = "playmusic" sp+ string_param
    stop_music = "stopmusic" 
    show_dialogue_box = "showdialogue"
    hide_dialogue_box = "hidedialogue"
    single_line_javascript = "js" sp+ (~nl any)+
    if = "if" sp+ (~nl any)+
    else_if = ("elseif" | "elif") sp+ (~nl any)+
    else = "else"
    end_if = "endif"
    jump = "jump" sp+ string_param
    hide_image_map = "hideimagemap" sp+ float_param
    hide_permanent_notification = "hidestatus"
"""
(* We cannot get interpolation to work with a triple-quoted string. *)
        + dialogue_pattern
        + """
/* Multi-line patterns. */

    comment = single_line_comment | multi_line_comment
    single_line_comment = "//" (~nl any)*
    multi_line_comment = "/*" (~"*/" any)* "*/"

    multi_line_javascript = "js" (~"endjs" any)* "endjs"

    menu = "menu" sp+ string_param sp+ (~nl any)* nl menu_items end_menu
/* Note nl gets its own parameter in the semantics even though it is part of a group. */
    menu_items = ((menu_item | comment | menu_empty_line) nl)+
    menu_empty_line = sp*
    menu_item = int_param sp+ (~nl ~(sp+ "/") any)+ menu_item_conditional?
    menu_item_conditional = sp+ "/" sp+ (~nl any)+
    end_menu = sp* "endmenu"

/* TODO1 #image_map #future
- Allow clicking on an image map item to be associated with some JS action.
- Allow clicking on an image map item to not hide the image map.
*/
    image_map = "imagemap" sp+ string_param sp+ string_param sp+ float_param nl image_map_items end_image_map
    image_map_items = ((image_map_item | comment | image_map_empty_line) nl)+
    image_map_empty_line = sp*
    image_map_item = int_param sp+ int_param sp+ int_param sp+ int_param sp+ int_param image_map_item_conditional?
    image_map_item_conditional = sp+ "/" sp+ (~nl any)+
    end_image_map = sp* "endimagemap"

/* TODO2 #parsing In case the author forgets to close the notify block, we might warn them (with alert = false) if we find known commands inside it. */
    temporary_notification = "notify" sp+ (~"endnotify" any)* "endnotify"
    permanent_notification = "status" sp+ (~"endstatus" any)* "endstatus"

    eval = "eval" (~"endeval" any)* "endeval"
}
"""

(* TODO2 Utility function to get keywords list from grammar.
We skip non-alphanumeric keywords because character short names must consist of alphanumeric characters only.
*)
(*
    let keywords =
        System.Text.RegularExpressions.Regex.Matches(grammar_text, "\"(\\w+)\"")
            |> Seq.map (fun m -> m.Groups[1].Value)
            |> Set.ofSeq
            |> Set.toList
            |> json_stringify
    console.log keywords
*)

    grammar_text
