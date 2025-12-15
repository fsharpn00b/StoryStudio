module Parser_2

// String
open System

open Character_Types
open Command_Types
open Menu
open Log
open Parser_1_Match_Functions
open Parser_2_Helpers
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Parser_2"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions - parsing *)

let private handle_command (acc : Parser_Accumulator) (command_1 : Command) (next_token : Command_Pre_Parse) : Parser_Accumulator =
    let next_id_for_command, next_available_id = get_next_command_id next_token acc.parent_command_ids acc.current_command_id
    {
        acc with
            scene = acc.scene.Add (acc.current_command_id, {
                id = acc.current_command_id
                next_command_id = next_id_for_command
                parent_command_id = match acc.parent_command_ids with | head :: _ -> Some head | _ -> None
                command = Command_Post_Parse_Type.Command command_1
            })
            current_command_id = next_available_id
    }

let private handle_menu (acc : Parser_Accumulator) (menu : Menu_Data) (next_token : Command_Pre_Parse) : Parser_Accumulator =
    let next_id_for_command, next_available_id = get_next_command_id next_token acc.parent_command_ids acc.current_command_id
    {
        acc with
            scene = acc.scene.Add (acc.current_command_id, {
                id = acc.current_command_id
                next_command_id = next_id_for_command
                parent_command_id = match acc.parent_command_ids with | head :: _ -> Some head | _ -> None
                command = Command_Post_Parse_Type.Menu menu
            })
            current_command_id = next_available_id
    }

let private handle_if (acc : Parser_Accumulator) (conditional : string) : Parser_Accumulator =
(* Reserve the next available ID for the End_If token. That way, when we encounter the End_If token, we can assign its ID by getting the ID of its parent, which is this If token, then adding 1. *)
    let next_command_id = acc.current_command_id + 1<command_id>
(* Use the ID after next_command_id for the first command in the If block. *)
    let child_command_id = next_command_id + 1<command_id>
    {
        scene = acc.scene.Add (acc.current_command_id, {
            id = acc.current_command_id
            next_command_id = Some next_command_id
            parent_command_id = match acc.parent_command_ids with | head :: _ -> Some head | _ -> None
            command = Command_Post_Parse_Type.If {
                conditional = conditional
                child_command_id = child_command_id
                else_if_blocks = []
                else_block = None
            }
        })
        current_command_id = child_command_id
(* Set the parent ID for the If block's commands to the current ID. *)
        parent_command_ids = acc.current_command_id :: acc.parent_command_ids
    }

let private handle_else_if (acc : Parser_Accumulator) (conditional : string) : Parser_Accumulator =
    let child_command_id = acc.current_command_id + 1<command_id>
(* Get the If block to which this Else_If token belongs. Add the Else_If branch to the If block. *)
    let parent, parent_command_id, if_block = get_parent_if acc.scene acc.parent_command_ids
    let if_block = {
        if_block with
            else_if_blocks = if_block.else_if_blocks @ [{ conditional = conditional; child_command_id = child_command_id }]
    }
    {
        acc with
            scene = acc.scene.Add (parent_command_id, {
                parent with command = Command_Post_Parse_Type.If if_block
            })
            current_command_id = child_command_id
    }

let private handle_else (acc : Parser_Accumulator) : Parser_Accumulator = 
    let child_command_id = acc.current_command_id + 1<command_id>
(* Get the If block to which this Else token belongs. Add the Else branch to the If block. *)
    let parent, parent_command_id, if_block = get_parent_if acc.scene acc.parent_command_ids
    if not if_block.else_block.IsNone then
        error "handle_else" "If block already has an Else block." ["if_block", if_block; "scene", acc.scene] |> invalidOp
    let if_block = {
        if_block with
            else_block = Some child_command_id
    }
    {
        acc with
            scene = acc.scene.Add (parent_command_id, {
                parent with command = Command_Post_Parse_Type.If if_block
            })
            current_command_id = child_command_id
    }

let private handle_end_if (acc : Parser_Accumulator) (next_token : Command_Pre_Parse) : Parser_Accumulator =
(* Get the ID for the If token to which this End_If token corresponds. Remove the ID from parent_command_ids. *)
    let parent_command_id, parent_command_ids =
        match acc.parent_command_ids with
        | head :: tail -> head, tail
        | _ -> error "handle_end_if" "Tried to get parent ID (If block ID) for EndIf, but there is no parent ID." ["scene", acc.scene; "current_command_id", acc.current_command_id; "next_token", next_token] |> invalidOp
(* parent_command_id + 1 is the ID we reserved earlier for the End_If. *)
    let id_for_command = parent_command_id + 1<command_id>
    let next_id_for_command, next_available_id = get_next_command_id next_token parent_command_ids acc.current_command_id
    {
        acc with
            scene = acc.scene.Add (id_for_command, {
                id = id_for_command
                next_command_id = next_id_for_command
                parent_command_id = Some parent_command_id
                command = Command_Post_Parse_Type.End_If
            })
            current_command_id = next_available_id
            parent_command_ids = parent_command_ids
    }

let parse_commands
    (tokens_1: Command_Pre_Parse list)
    : Scene =

    let initial_value = {
        scene = Map.empty
        current_command_id = 1<command_id>
        parent_command_ids = []
    }

    let result = (initial_value, tokens_1 |> List.pairwise) ||> List.fold (fun acc (token, next_token) ->
        do check_current_token_and_next_token token next_token
        match token with
        | Command_Pre_Parse.Command command -> handle_command acc command next_token
        | Command_Pre_Parse.If conditional -> handle_if acc conditional
        | Command_Pre_Parse.Else_If conditional -> handle_else_if acc conditional
        | Command_Pre_Parse.Else -> handle_else acc
        | Command_Pre_Parse.End_If -> handle_end_if acc next_token
        | Command_Pre_Parse.Menu menu -> handle_menu acc menu next_token
    )

(* The last token should be either a command or End_If. *)
    match List.last tokens_1 with

    | Command_Pre_Parse.Command command ->
        if result.parent_command_ids.Length > 0 then
            error "parse_commands" "Parser reached last command, which is not EndIf, but there are still parent_command_ids, meaning an If block was not closed." ["parent_command_ids", result.parent_command_ids; "command", command; "scene", result.scene] |> invalidOp
        let scene = result.scene.Add (result.current_command_id, {
            id = result.current_command_id
            next_command_id = None
            parent_command_id = None
            command = Command_Post_Parse_Type.Command command
        })
        scene

    | Command_Pre_Parse.End_If ->
        let parent_command_id =
            match result.parent_command_ids with
            | head :: [] -> head
            | _ ->
                error "parse_commands" "Parser reached last command, which is EndIf, but parent_command_ids has length <> 1, meaning either an If block was not closed, or the End_If does not have a corresponding If block." ["parent_command_ids", result.parent_command_ids; "scene", result.scene] |> invalidOp
(* parent_command_id + 1 is the ID we reserved earlier for the End_If. *)
        let id_for_command = parent_command_id + 1<command_id>
        let scene = result.scene.Add (id_for_command, {
            id = id_for_command
            next_command_id = None
            parent_command_id = Some parent_command_id
            command = Command_Post_Parse_Type.End_If
        })
        scene

    | _ -> error "parse" "Invalid last token. Last token should be a command or EndIf." ["Last token", List.last tokens_1 :> obj] |> invalidOp

let get_scene_map_and_javascript
    (scripts : Script list)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    : Scene_Map =

    scripts
        |> List.map (fun script ->
            script.id, script.content.Split Environment.NewLine
                |> Array.toList
                |> match_commands backgrounds characters scripts
                |> parse_commands
        )
        |> Map.ofList

(* TODO2 We can also add while easily. Just have While_End have While as its next_command_id.
*)
