module Parser_2_1

// Environment.NewLine
open System

// console, window
open Browser.Dom

open Command_Types
open Log
open Parser_1_Helpers
open Parser_2_Helpers
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Parser_2_1"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions - parsing *)

let handle_command
    (acc : Parser_2_Accumulator)
    (scene_id : int<scene_id>)
    (command : Command_Post_Parse_Type)
    (token : Command_Pre_Parse_2)
    (next_token : Command_Pre_Parse_2 option)
    : Parser_2_Accumulator =

    let current_command_id = acc.next_command_id_to_assign

    let next_command_data_1, next_available_id =
        match next_token with
        | Some next_token ->
            let next_command_id_data = get_next_command_id next_token acc.parent_command_ids current_command_id
            let next_command_data_2 =
                match token.command with
                | Command_Pre_Parse_Type.Jump_Scene data
                | Command_Pre_Parse_Type.Jump_Internal data ->
                    Some {
                       next_command_scene_id = data.scene_id
                       next_command_id = data.command_id
                    }
                | Command_Pre_Parse_Type.Jump_Label _ -> None
                | _ ->
                    Some {
                        next_command_scene_id = scene_id
                        next_command_id = next_command_id_data.next_id_for_command
                    }
            next_command_data_2, next_command_id_data.next_available_id
(* In Parser_2_2.parse_commands_2 (), we have already made sure the last token is not If, Else If, or Else. *)

        | None ->
            let next_command_data_2 =
                match token.command with
                | Command_Pre_Parse_Type.Jump_Scene data
                | Command_Pre_Parse_Type.Jump_Internal data ->
                    Some {
                        next_command_scene_id = data.scene_id
                        next_command_id = data.command_id
                    }
                | Command_Pre_Parse_Type.Jump_Label _ -> None
                | _ -> None
(* If the next token is None, the next available ID is irrelevant. We should:
1 Return an optional anonymous record that contains both Next_Command_Data and the next available ID.
2 Make Parser_2_Accumulator.next_command_id_to_assign optional.
(end)
However, we then have to match Parser_2_Accumulator.next_command_id_to_assign to get the value for every token we parse, which is more trouble than it is worth. Instead, we just do not use the final value of Parser_2_Accumulator.next_command_id_to_assign.
*)
            next_command_data_2, current_command_id + 1<command_id>
    let parent_command_id = match acc.parent_command_ids with | head :: _ -> Some head | _ -> None

    {
        acc with
            scene = {
                acc.scene with
                    commands = acc.scene.commands.Add (current_command_id, {
                        id = current_command_id
                        next_command_data = next_command_data_1
                        parent_command_id = parent_command_id
                        error_data = token.error_data
                        command = command
                    })
            }
            next_command_id_to_assign = next_available_id
            labels =
                match token.command with
                | Command_Pre_Parse_Type.Label label -> acc.labels.Add (label, current_command_id)
                | _ -> acc.labels
            jump_label_commands =
                match token.command with
                | Command_Pre_Parse_Type.Jump_Label label -> acc.jump_label_commands.Add (current_command_id, label)
                | _ -> acc.jump_label_commands
    }

let handle_if
    (acc : Parser_2_Accumulator)
    (scene_id : int<scene_id>)
    (token : Command_Pre_Parse_2)
    (conditional : string)
    : Parser_2_Accumulator =

    let current_command_id = acc.next_command_id_to_assign
(* Reserve the next available ID for the End_If token. That way, when we encounter the End_If token, we can assign its ID by getting the ID of its parent, which is this If token, then adding 1. *)
    let next_command_id = current_command_id + 1<command_id>
(* Use the ID after next_command_id for the first command in the If block. *)
    let child_command_id = next_command_id + 1<command_id>
    let command = Command_Post_Parse_Type.If {
        conditional = conditional
        child_command_id = child_command_id
        else_if_blocks = []
        else_block = None
    }
    let parent_command_id = match acc.parent_command_ids with | head :: _ -> Some head | _ -> None
(* Set the parent ID for the If block's commands to the current ID. *)
    let parent_command_ids = current_command_id :: acc.parent_command_ids

    {
        acc with
            scene = {
                acc.scene with
                    commands = acc.scene.commands.Add (current_command_id, {
                        id = current_command_id
                        next_command_data =
                            Some {
                                next_command_scene_id = scene_id
                                next_command_id = next_command_id
                            }
                        parent_command_id = parent_command_id
                        error_data = token.error_data
                        command = command
                    })
            }
            next_command_id_to_assign = child_command_id
(* Set the parent ID for the If block's commands to the current ID. *)
            parent_command_ids = parent_command_ids
    }

let handle_else_if
    (acc : Parser_2_Accumulator)
    (source : string)
    (conditional : string)
    : Parser_2_Accumulator =

    let current_command_id = acc.next_command_id_to_assign
    let child_command_id = current_command_id + 1<command_id>
(* Get the If block to which this Else_If token belongs. Add the Else_If branch to the If block. *)
    let result = get_parent_if acc.scene acc.parent_command_ids
    let parent, parent_command_id, if_block = result.parent, result.parent_command_id, result.if_block
    let if_block = {
        if_block with
            else_if_blocks = if_block.else_if_blocks @ [{ conditional = conditional; child_command_id = child_command_id }]
    }

    {
        acc with
            scene = {
                acc.scene with
                    commands = acc.scene.commands.Add (parent_command_id, {
                        parent with
                            error_data =
                                {
                                    parent.error_data with
                                        source = $"{parent.error_data.source}{Environment.NewLine}{source}"  
                                }
                            command = Command_Post_Parse_Type.If if_block
                    })
            }
            next_command_id_to_assign = child_command_id
    }

let handle_else
    (acc : Parser_2_Accumulator)
    : Parser_2_Accumulator =

    let current_command_id = acc.next_command_id_to_assign
    let child_command_id = current_command_id + 1<command_id>
(* Get the If block to which this Else token belongs. Add the Else branch to the If block. *)
    let result = get_parent_if acc.scene acc.parent_command_ids
    let parent, parent_command_id, if_block = result.parent, result.parent_command_id, result.if_block
    if not if_block.else_block.IsNone then
        error "handle_else" "If block already has an Else block." ["if_block", if_block; "scene", acc.scene] |> invalidOp
    let if_block = {
        if_block with
            else_block = Some child_command_id
    }

    {
        acc with
            scene = {
                acc.scene with
                    commands = acc.scene.commands.Add (parent_command_id, {
                        parent with
                            error_data =
                                {
                                    parent.error_data with
(* If can have only one Else, so we go ahead and add End_If as well here. *)
                                        source = $"{parent.error_data.source}{Environment.NewLine}else{Environment.NewLine}endif"
                                }
                            command = Command_Post_Parse_Type.If if_block
                    })
            }
            next_command_id_to_assign = child_command_id
    }

let handle_end_if
    (acc : Parser_2_Accumulator)
    (scene_id : int<scene_id>)
    (token : Command_Pre_Parse_2)
    (next_token_1 : Command_Pre_Parse_2 option)
    : Parser_2_Accumulator =

    let current_command_id = acc.next_command_id_to_assign
(* Get the ID for the If token to which this End_If token corresponds. Remove the ID from parent_command_ids. *)
    let parent_command_id, parent_command_ids =
        match acc.parent_command_ids with
        | head :: tail -> head, tail
        | _ -> error "handle_end_if" "Tried to get parent ID (If block ID) for EndIf, but there is no parent ID." ["scene", acc.scene; "current_command_id", current_command_id; "next_token", next_token_1] |> invalidOp
(* parent_command_id + 1 is the ID we reserved earlier for the End_If. *)
    let id_for_command = parent_command_id + 1<command_id>

    let next_command_id_1, next_available_id =
        match next_token_1 with
        | Some next_token_2 ->
            let result = get_next_command_id next_token_2 parent_command_ids current_command_id
            Some result.next_id_for_command, result.next_available_id
        | None -> None, current_command_id

    {
        acc with
            scene = {
                acc.scene with
                    commands = acc.scene.commands.Add (id_for_command, {
                        id = id_for_command
                        next_command_data =
                            match next_command_id_1 with
                            | Some next_command_id_2 ->
                                Some {
                                    next_command_scene_id = scene_id
                                    next_command_id = next_command_id_2
                                }
                            | None -> None                        
                        parent_command_id = Some parent_command_id
                        error_data = token.error_data
                        command = Command_Post_Parse_Type.End_If
                    })
            }
            next_command_id_to_assign = next_available_id
            parent_command_ids = parent_command_ids
    }
