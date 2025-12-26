module Parser_1_Match_Functions

// Environment.NewLine
open System

open Character_Types
open Command_Types
open Image_Map
open Menu
open Log
open Parser_1_Match_Patterns

(* Debug *)

let debug_module_name = "Parser_1_Match_Functions"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Helper functions - parsing *)

let private prepare_commands (commands_1 : string list) : string list =
    let commands_2 =
        commands_1
            |> List.map (fun command -> command.Trim ())
            |> List.filter (fun command -> command.Length > 0)
    if List.isEmpty commands_2 then
        error "prepare_commands" "Command list is empty after removing zero-length commands." ["commands_1", "commands_1"] |> invalidOp
    else commands_2

let rec private collect_multi_line_comment
    (remaining_commands : string list)
    : string list =
    match remaining_commands with
    | [] -> error "collect_multi_line_comment" "Comment block never terminates." [] |> invalidOp
    | head :: tail ->
        if head |> multi_line_comment_end_regex.IsMatch then tail
        else collect_multi_line_comment tail

(* We handle JavaScript blocks here, rather than in pre_nesting_commands_to_post_nesting_commands, because JavaScript statements do not conform to any pattern. When we find a JavaScript start command, we simply record the following commands and add them to the JavaScript block, until we find a JavaScript end command. *)
(* We do not need to discard comments in JavaScript blocks. The browser will handle those when we eval the JavaScript. *)
let rec private collect_javascript
    (javascript_acc : string list)
    (remaining_commands : string list)
    : {|
        command : Command
        remaining_commands : string list
    |} =
    match remaining_commands with
    | [] -> error "collect_javascript" "JavaScript block never terminates." ["acc", javascript_acc] |> invalidOp
    | head :: tail ->
        if head |> javascript_end_regex.IsMatch then
            match javascript_acc with
            | [] -> error "collect_javascript" "JavaScript block is empty." [] |> invalidOp
            | _ ->
                {|
(* We append each line to the accumulator, so reverse the accumulator before returning it. *)
                    command = javascript_acc |> List.rev |> String.concat Environment.NewLine |> JavaScript_Block                    
                    remaining_commands = tail
                |}
        else collect_javascript (head :: javascript_acc) tail

let rec private collect_menu
    (menu_1_acc : Menu_Data)
    (remaining_commands : string list)
    : {|
        menu_data : Menu_Data
        remaining_commands : string list
    |} =
    match remaining_commands with
    | [] -> error "collect_menu" "Menu block never terminates." ["menu", menu_1_acc] |> invalidOp
    | head :: tail ->
(* Discard single-line comments. *)
        if head |> single_line_comment_regex.IsMatch then
            collect_menu menu_1_acc tail
(* Discard multi-line comments. *)
        elif head |> multi_line_comment_start_regex.IsMatch then
            let remaining_commands = collect_multi_line_comment tail
            collect_menu menu_1_acc remaining_commands
        elif head |> end_menu_regex.IsMatch then
            if 0 = List.length menu_1_acc.items then
                error "collect_menu" "Menu contains no items." ["menu", menu_1_acc] |> invalidOp
            elif menu_1_acc.items |> List.forall (fun item -> item.conditional.IsSome) then
                error "collect_menu" "Menu contains only conditional items. At least one item must be non-conditional." ["menu", menu_1_acc] |> invalidOp
            else {|
(* We append each menu item to the accumulator, so reverse the accumulator before returning it. *)
                menu_data = { menu_1_acc with items = menu_1_acc.items |> List.rev }
                remaining_commands = tail
            |}
        elif head |> menu_item_regex.IsMatch then
            let menu_item = (match_menu_item head).Value
            let menu_2 = { menu_1_acc with items = menu_item :: menu_1_acc.items }
            collect_menu menu_2 tail
        else error "collect_menu" "While parsing menu block, encountered line that is neither menu item nor EndMenu." ["line", head; "menu", menu_1_acc] |> invalidOp

let rec private collect_image_map
    (image_map_1_acc : Image_Map_Data)
    (remaining_commands : string list)
    : {|
        image_map_data : Image_Map_Data
        remaining_commands : string list
    |} =
    match remaining_commands with
    | [] -> error "collect_image_map" "ImageMap block never terminates." ["image_map", image_map_1_acc] |> invalidOp
    | head :: tail ->
(* Discard single-line comments. *)
        if head |> single_line_comment_regex.IsMatch then
            collect_image_map image_map_1_acc tail
(* Discard multi-line comments. *)
        elif head |> multi_line_comment_start_regex.IsMatch then
            let remaining_commands = collect_multi_line_comment tail
            collect_image_map image_map_1_acc remaining_commands
        elif head |> end_image_map_regex.IsMatch then
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
        elif head |> image_map_item_regex.IsMatch then
            let image_map_item = (match_image_map_item head).Value
            let image_map_2 = { image_map_1_acc with items = image_map_item :: image_map_1_acc.items }
            collect_image_map image_map_2 tail
        else error "collect_image_map" "While parsing image_map block, encountered line that is neither image map item nor EndImageMap." ["line", head; "image_map", image_map_1_acc] |> invalidOp

(* Main functions - matching *)

(* Try to match the command using our patterns in descending order of priority and/or increasing order of generality. *)
let private match_command
    (command : string)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    (scenes : Script list)
    : Command_Pre_Parse =
    let attempts = seq {
(* We handle menus and JavaScript blocks separately. *)
        match_music_play command
        match_music_stop command
        match_background_fade_in command backgrounds
        match_background_fade_out command
        match_background_cross_fade command backgrounds
        match_character_fade_in command characters
        match_character_fade_out command characters
        match_character_cross_fade command characters
        match_fade_out_all command
        match_dialogue_box_show command
        match_dialogue_box_hide command
        match_javascript_inline command
        match_if_start command
        match_else_if command
        match_else command
        match_if_end command
        match_jump command scenes
        match_dialogue command characters
    }
    match Seq.tryPick id attempts with
    | Some result -> result
    | None -> error "match_command" "Unrecognized command. This might also be due to a dialogue command with an unrecognized character, as the parser tries to parse dialogue last." ["command", command] |> invalidOp

let match_commands
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    (scenes : Script list)
    (commands : string list)
    : Command_Pre_Parse list =

    let rec helper (command_acc : Command_Pre_Parse list) (commands : string list) : string list * Command_Pre_Parse list =
        match commands with
(* We append each command to the accumulator, so reverse the accumulator before returning it. *)
        | [] -> [], command_acc |> List.rev
        | head :: tail ->
(* Discard single-line comments. *)
            if head |> single_line_comment_regex.IsMatch then
                helper command_acc tail
(* Discard multi-line comments. *)
            elif head |> multi_line_comment_start_regex.IsMatch then
                let remaining_commands = collect_multi_line_comment tail
                helper command_acc remaining_commands
(* If we encounter a javascript block, collect the statements in the block. *)
            elif head |> javascript_start_regex.IsMatch then
                let result = collect_javascript [] tail
                helper (Command_Pre_Parse.Command result.command :: command_acc) result.remaining_commands
(* If we encounter a menu block, collect the statements in the block. *)
            elif head |> menu_start_regex.IsMatch then
                let menu_data = (match_menu_start head).Value
                let result = collect_menu menu_data tail
                helper (Command_Pre_Parse.Menu result.menu_data :: command_acc) result.remaining_commands
(* If we encounter an image map block, collect the statements in the block. *)
            elif head |> image_map_start_regex.IsMatch then
                let image_map_data = (match_image_map_start head backgrounds).Value
                let result = collect_image_map image_map_data tail
                helper (Command_Pre_Parse.Image_Map result.image_map_data :: command_acc) result.remaining_commands
(* We handle end_image_map separately and use it to fade out the image map. *)
            elif head |> end_image_map_regex.IsMatch then 
                helper (Command_Pre_Parse.End_Image_Map (match_image_map_end head).Value :: command_acc) tail
(* Otherwise, determine what kind of command this is. *)
            else
                helper ((match_command head backgrounds characters scenes) :: command_acc) tail

    commands |> prepare_commands |> helper [] |> snd
