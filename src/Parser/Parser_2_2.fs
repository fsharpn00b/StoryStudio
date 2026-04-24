module Parser_2_2

// Environment.NewLine
open System

// console, window
open Browser.Dom

open Command_Types
open Log
open Parser_1_Helpers
open Parser_2_Helpers
open Parser_2_1
open Scripts
open Units_Of_Measure
(* This is used in debug mode.*)
open Utilities

(* Debug *)

let debug_module_name = "Parser_2_2"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions - parsing *)

let private parse_jump_label_commands
    (accumulator : Parser_2_Accumulator)
    (scene_id : int<scene_id>)
(* These are for error reporting. *)
    (script_content : string)
    (script_name : string)
    (scripts : Script list)
    : Parser_2_Accumulator =

    (accumulator, accumulator.jump_label_commands) ||> Seq.fold (fun acc kv ->
        let command_1 = acc.scene.commands.[kv.Key]
        let command_2 =
            {
                command_1 with
                    next_command_data = Some {
                        next_command_scene_id = scene_id
                        next_command_id =
                            match resolve_jump_label_destination accumulator.labels kv.Value with
                            | Some command_id -> command_id
                            | None ->
                                let line_number = get_script_line_number script_content command_1.error_data.script_text_index
                                error "parse_commands" $"A jump command does not point to any known scene or label." ["scene_name", script_name; "line_number", line_number; "jump_destination", kv.Value; "known_labels_in_current_scene", accumulator.labels |> Seq.map (fun kv -> kv.Value) :> obj; "known_scenes", scripts |> List.map (fun script -> script.name) :> obj] |> invalidOp
                    }
            }
        {
            acc with
                scene = {
                    acc.scene with
(* This overwrites the placeholder Jump_Label command *)
                        commands = acc.scene.commands.Add (kv.Key, command_2)
                }
        })

let private parse_commands_2
    (accumulator : Parser_2_Accumulator)
    (scene_id : int<scene_id>)
    (token_pairs_with_lookahead : (Command_Pre_Parse_2 * Command_Pre_Parse_2 option) array)
    : Parser_2_Accumulator =

    (accumulator, token_pairs_with_lookahead) ||> Array.fold (fun acc (token, next_token) ->

        #if debug
        do debug "parse_commands" String.Empty ["current_command_id", accumulator.next_command_id_to_assign; "parent_command_ids", accumulator.parent_command_ids; "token", json_stringify token; "next_token", json_stringify next_token]
        #endif

        if next_token.IsNone && not (is_valid_terminal_token token.command) then
            error "parse_commands" "If/ElseIf/Else cannot be the last token in a script." ["token", token] |> invalidOp

        match next_token with
        | Some next_token -> check_current_token_and_next_token token next_token
        | None -> ()

        match token.command with
        | Command_Pre_Parse_Type.Command command -> handle_command acc scene_id (Command_Post_Parse_Type.Command command) token next_token
        | Command_Pre_Parse_Type.If conditional -> handle_if acc scene_id token conditional
        | Command_Pre_Parse_Type.Else_If conditional -> handle_else_if acc token.error_data.source conditional
        | Command_Pre_Parse_Type.Else -> handle_else acc
        | Command_Pre_Parse_Type.End_If -> handle_end_if acc scene_id token next_token
        | Command_Pre_Parse_Type.Menu menu -> handle_command acc scene_id (Command_Post_Parse_Type.Menu menu) token next_token
        | Command_Pre_Parse_Type.Image_Map image_map_data -> handle_command acc scene_id (Command_Post_Parse_Type.Image_Map image_map_data) token next_token
        | Command_Pre_Parse_Type.End_Image_Map transition_time -> handle_command acc scene_id (Command_Post_Parse_Type.End_Image_Map transition_time) token next_token
        | Command_Pre_Parse_Type.Jump_Scene _ -> handle_command acc scene_id (Command_Post_Parse_Type.Jump_Scene) token next_token
        | Command_Pre_Parse_Type.Jump_Label _ -> handle_command acc scene_id (Command_Post_Parse_Type.Jump_Label) token next_token
        | Command_Pre_Parse_Type.Jump_Internal _ -> handle_command acc scene_id (Command_Post_Parse_Type.Jump_Internal) token next_token
        | Command_Pre_Parse_Type.Label _ -> handle_command acc scene_id (Command_Post_Parse_Type.Label) token next_token
    )

let private parse_commands_1
    (scene_id : int<scene_id>)
    (script_name : string)
    (script_content : string)
    (tokens_1: Command_Pre_Parse_2 list)
(* This is for error reporting. *)
    (scripts : Script list)
    : Scene_Data =

    let accumulator_1 = {
        scene = {
            commands = Map.empty
            name = script_name
            content = script_content
        }
        next_command_id_to_assign = scene_initial_command_id
        parent_command_ids = []
        labels = Map.empty
        jump_label_commands = Map.empty
    }

    let tokens_2 = tokens_1 |> List.toArray
    let token_pairs_with_lookahead =
        tokens_2
        |> Array.mapi (fun i token ->
            let next_token = if i + 1 < tokens_2.Length then Some tokens_2.[i + 1] else None
            token, next_token
        )

    let accumulator_2 = parse_commands_2 accumulator_1 scene_id token_pairs_with_lookahead
    let accumulator_3 = parse_jump_label_commands accumulator_2 scene_id script_content script_name scripts

    if accumulator_3.parent_command_ids.Length > 0 then
        error "parse_commands" "Parser reached end of script, but there are still parent_command_ids, meaning an If block was not closed." ["parent_command_ids", accumulator_3.parent_command_ids; "scene", accumulator_3.scene] |> invalidOp

    accumulator_3.scene

let get_scene_map
    (parser : Parser)
    (scripts : Script list)
    : Scene_Map =

    scripts
        |> List.map (fun script ->
(* parse_script_1 simply wraps parse_script_2 in a try/catch block. *)
            match parser script with
            | [] -> error "get_scene_map_and_javascript" "Script is empty or contains only comments." ["script name", script.name] |> invalidOp
            | commands -> script.id, parse_commands_1 script.id script.name script.content commands scripts
        )
        |> Map.ofList

(* TODO2 We can also add while easily. Just have While_End have While as its next_command_id.
*)
