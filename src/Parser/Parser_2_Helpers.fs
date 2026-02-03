module Parser_2_Helpers

// console, window
open Browser.Dom

open Command_Types
open Log
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Parser_2_Helpers"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

type Parser_2_Accumulator = {
    scene : Scene_Data
    current_command_id : int<command_id>
    parent_command_ids : int<command_id> list
}

(* Helper functions - parsing *)

(* This function checks for invalid token sequences involving If/Else_If/Else, such as If followed by Else_If with no commands.
For Menu and Image_Map, we check for invalid token sequences with Parser_1_Match_Functions.collect_menu () and collect_image_map (). Menu and Image_Map cannot have nested commands, whereas If/Else_If/Else can.
Parser_1_Match_Patterns.match_if_start (), match_else_if (), match_else (), and match_if_end () do not check for invalid token sequences. They just get the corresponding conditionals.
*)
let check_current_token_and_next_token (token : Command_Pre_Parse) (next_token : Command_Pre_Parse) : unit =
    let tokens = ["token", token :> obj; "next_token", next_token :> obj]

    match token.command with

    | Command_Pre_Parse_Type.If _ ->
        match next_token.command with
        | Command_Pre_Parse_Type.Command _
        | Command_Pre_Parse_Type.If _ -> ()
        | _ -> error "check_current_token_and_next_token" "If must be followed by command or another If." tokens |> invalidOp

    | Command_Pre_Parse_Type.Else_If _ ->
        match next_token.command with
        | Command_Pre_Parse_Type.Command _
        | Command_Pre_Parse_Type.If _ -> ()
        | _ -> error "check_current_token_and_next_token" "ElseIf must be followed by command or If." tokens |> invalidOp

    | Command_Pre_Parse_Type.Else ->
        match next_token.command with
        | Command_Pre_Parse_Type.Command _
        | Command_Pre_Parse_Type.If _ -> ()
        | _ -> error "check_current_token_and_next_token" "Else must be followed by command or another If." tokens |> invalidOp

    | _ -> ()

(* The first return parameter is the next_command_id for the current command. The second is the ID to be used for the next command. These are not always the same. See the Else_If/Else/End_If case.
*)
let get_next_command_id (next_token : Command_Pre_Parse) (parent_command_ids : int<command_id> list) (id : int<command_id>) : int<command_id> option * int<command_id> =
    match next_token.command with

    | Command_Pre_Parse_Type.Command _
    | Command_Pre_Parse_Type.If _
    | Command_Pre_Parse_Type.Menu _
    | Command_Pre_Parse_Type.Image_Map _
    | Command_Pre_Parse_Type.End_Image_Map _ -> Some <| id + 1<command_id>, id + 1<command_id>
    | Command_Pre_Parse_Type.Else_If _
    | Command_Pre_Parse_Type.Else
    | Command_Pre_Parse_Type.End_If ->
        let parent_command_id =
            match parent_command_ids with
            | head :: _ -> head
            | _ -> error "get_next_command_id" "The next token is elseif, else, or endif, which means we are in a branch, but there is no parent ID. This probably means the script has an endif statement with no corresponding if statement. There might be an if statement enclosed in a comment or notify/status block that was not closed correctly." ["next_token", next_token] |> invalidOp
(* When we encounter If as the current token, we set its ID and also reserve the following ID for the End_If.
Later, when we encounter Else_If, Else or End_If at the next token, we terminate the branch we are on by setting next_command_id to the ID we reserved for the End_If.

For example:

ID  Token       next_command_id     parent_command_id       child_command_id
1   If          2                   ?                       3
3   (command)   2                   1
2   End_If      4                   1
4   ...

When we encounter Else_If, Else, or End_If as the next token, we return parent ID + 1 (2) as the next ID for the current token, and current ID as the next ID to use after the End_If. We do not need to increment the current ID because we do not create commands for Else_If or Else, and we have already reserved an ID for End_If.
When we encounter End_If as the current token, we set its ID to the previously reserved ID, 2, and do not increment the current ID, 4.
*)
        Some <| parent_command_id + 1<command_id>, id
(* We do not use this for now. *)
(*
    | _ -> None, id
*)

(* Get the parent If block for an Else_If or Else token. *)
let get_parent_if (command_map : Scene_Data) (parent_command_ids : int<command_id> list) : Command_Post_Parse * int<command_id> * If_Block =
    let parent_command_id =
        match parent_command_ids with
        | head :: _ -> head
        | _ -> error "get_parent_if" "Tried to get parent ID (If block ID) for ElseIf, Else, or EndIf, but there is no parent ID." ["command_map", command_map] |> invalidOp
    let parent =
        match command_map.commands.TryFind parent_command_id with
        | Some parent -> parent
        | None -> error "get_parent_if" "Tried to get parent (If block) for ElseIf, Else, or EndIf, but the parent was not found." ["parent_command_id", parent_command_id; "command_map", command_map] |> invalidOp
    match parent.command with
    | Command_Post_Parse_Type.If if_block -> parent, parent_command_id, if_block
    | _ -> error "get_parent_if" "Tried to get parent for ElseIf, Else, or EndIf, but the parent is not an If block." ["parent", parent; "parent_command_id", parent_command_id; "command_map", command_map] |> invalidOp
