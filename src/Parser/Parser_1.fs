module Parser_1

// Double, Int32
open System

// TODO1 Put this in every file to make debugging easier.
// console, window
open Browser.Dom

open Character_Types
open Parser_Patterns
open Command_Types
open Log
open Parser_Command_Data
open Parser_1_Helpers
open Temporary_Notification

(* Debug *)

let debug_module_name = "Parser_1"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

let private initial_command_parameters = {
    ints = Map.empty
    floats = Map.empty
    strings = Map.empty
}

(* Functions - main *)

let private match_command_parameters_for_single_overload 
    (overload : Command_Pattern_2)
    (parameters : string)
    : Command_Parameters option = 

    match overload.parameters_regex with

(* If the overload contains no parameters, then the author must provide an empty parameter list for this to be a match. *)
    | None ->
        if String.IsNullOrEmpty parameters then Some initial_command_parameters
        else None

    | Some regex ->
        let m = regex.Match parameters
        if m.Success then

            #if debug
            debug "match_command_parameters_for_single_overload" "Matched command overload." [
                "overload", overload
                "parameters", parameters
                "match_data", m.Groups
            ]
            #endif

(* Go through the expected parameters and get the value for each from the match result. *)
            (initial_command_parameters, overload.parameters) ||> Seq.fold (fun acc kv ->
                let parameter_name = kv.Key
                let parameter_value = m.Groups[parameter_name].Value

                match kv.Value with
                | Any_Min_Length_0
                | Any_Min_Length_1
(* We do not use this for now. *)
//                | Other _
                | Word -> { acc with strings = acc.strings.Add (parameter_name, parameter_value) }
(* A parameter of type Int has been matched by Parser_Patterns.int_pattern. *)
                | Int -> { acc with ints = acc.ints.Add (parameter_name, Int32.Parse parameter_value) }
(* A parameter of type Float has been matched by Parser_Patterns.float_pattern. *)
                | Float -> { acc with floats = acc.floats.Add (parameter_name, Double.Parse parameter_value ) }
            ) |> Some
(* A match failure is not necessarily an error. *)
        else None

let private match_command_and_parameters
    (command_patterns : Command_Pattern_2 list)
    (line_number : int)
    (command : string)
    (parameters : string)
    : Result<(Command_Name * Command_Parameters), Parser_Error> =

    let matching_command_patterns =
        command_patterns
            |> List.filter (fun command_pattern -> 0 = String.Compare (command_pattern.pattern, command))

    if List.isEmpty matching_command_patterns then
        Error ("No matching commands found. This might also be due to a dialogue command with an unrecognized character.", [
            "line_number", line_number
            "line", $"{command} {parameters}"
        ])
    else
        let result_1 =
            matching_command_patterns |> List.tryPick (fun pattern ->
                match match_command_parameters_for_single_overload pattern parameters with
                | Some result -> Some (pattern.name, result)
                | None -> None
            )
        match result_1 with
        | Some result_2 -> Ok result_2
        | None -> Error ("Matching commands found, but the parameters provided do not match the signatures of any of these commands.", [
            "line_number", line_number
            "line", $"{command} {parameters}"
            "matching commands", matching_command_patterns |> List.map command_pattern_2_to_signature :> obj // |> List.reduce (fun acc item -> $"{acc}{Environment.NewLine}{item}") :> obj
        ])

let private matched_command_to_pre_parse_command
    (name : Command_Name)
    (parameters : Command_Parameters)
    (scenes : Script list)
    (music_tracks : Map<string, string>)
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    : Result<Command_Pre_Parse, Parser_Error> =

    match name with
    | CN_Music_Play ->
        match get_music music_tracks parameters.strings.["name"] with
        | Ok url -> Command_Types.Music_Play url |> Command_Pre_Parse.Command |> Ok
        | Error e -> Error e
    | CN_Music_Stop -> Command_Types.Music_Stop |> Command_Pre_Parse.Command |> Ok
    | CN_Background_Fade_In ->                 
        match get_background backgrounds parameters.strings.["name"] with
        | Ok url ->
            Command_Types.Background_Fade_In {
                new_url = url
                transition_time = LanguagePrimitives.FloatWithMeasure parameters.floats.["transition_time"]
            } |> Command_Pre_Parse.Command |> Ok
        | Error e -> Error e
    | CN_Background_Fade_Out ->                 
        Command_Types.Background_Fade_Out {
            transition_time = LanguagePrimitives.FloatWithMeasure parameters.floats.["transition_time"]
        } |> Command_Pre_Parse.Command |> Ok
    | CN_Background_Cross_Fade ->                 
        match get_background backgrounds parameters.strings.["name"] with
        | Ok url ->
            Command_Types.Background_Cross_Fade {
                new_url = url
                transition_time = LanguagePrimitives.FloatWithMeasure parameters.floats.["transition_time"]
            } |> Command_Pre_Parse.Command |> Ok
        | Error e -> Error e
    | CN_Character_Fade_In ->  
        match get_character_sprite characters parameters.strings.["name"] parameters.strings.["sprite"] with
        | Ok url ->
            Command_Types.Character_Fade_In {
                character_short_name = parameters.strings.["name"]
                url = url
                position = LanguagePrimitives.Int32WithMeasure parameters.ints.["position"]
                transition_time = LanguagePrimitives.FloatWithMeasure parameters.floats.["transition_time"]
            } |> Command_Pre_Parse.Command |> Ok
        | Error e -> Error e
    | CN_Character_Fade_Out ->  
        match get_character characters parameters.strings.["name"] with
(* We just need to verify the character name is valid. *)
        | Ok _ ->
            Command_Types.Character_Fade_Out {
                character_short_name = parameters.strings.["name"]
                transition_time = LanguagePrimitives.FloatWithMeasure parameters.floats.["transition_time"]
            } |> Command_Pre_Parse.Command |> Ok
        | Error e -> Error e
    | CN_Character_Cross_Fade ->  
        match get_character_sprite characters parameters.strings.["name"] parameters.strings.["sprite"] with
        | Ok url ->
            Command_Types.Character_Cross_Fade {
                character_short_name = parameters.strings.["name"]
                url = url
                transition_time = LanguagePrimitives.FloatWithMeasure parameters.floats.["transition_time"]
            } |> Command_Pre_Parse.Command |> Ok
        | Error e -> Error e
    | CN_Fade_Out_All ->
        parameters.floats.["transition_time"] |> LanguagePrimitives.FloatWithMeasure |> Command_Types.Fade_Out_All |> Command_Pre_Parse.Command |> Ok
    | CN_Dialogue_Box_Show -> Command_Types.Dialogue_Box_Show |> Command_Pre_Parse.Command |> Ok
    | CN_Dialogue_Box_Hide -> Command_Types.Dialogue_Box_Hide |> Command_Pre_Parse.Command |> Ok
    | CN_Temporary_Notification ->
        let text = parameters.strings.["text"]
        {
            text = text |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = text |> extract_javascript_interpolations
        } |> Temporary_Notification |> Command_Pre_Parse.Command |> Ok
    | CN_Permanent_Notification ->
        let text = parameters.strings.["text"]
        {
            text = text |> convert_string_to_use_javascript_interpolation
            javascript_interpolations = text |> extract_javascript_interpolations
        } |> Permanent_Notification |> Command_Pre_Parse.Command |> Ok
    | CN_JavaScript_Inline -> parameters.strings.["code"] |> Command_Types.JavaScript_Inline |> Command_Pre_Parse.Command |> Ok
    | CN_If -> parameters.strings.["conditional"] |> Command_Pre_Parse.If |> Ok
    | CN_Else_If -> parameters.strings.["conditional"] |> Command_Pre_Parse.Else_If |> Ok
    | CN_Else -> Command_Pre_Parse.Else |> Ok
    | CN_End_If -> Command_Pre_Parse.End_If |> Ok
    | CN_Jump ->
        match get_script_id scenes parameters.strings.["destination"] with
        | Ok scene_id -> scene_id |> Command_Types.Jump |> Command_Pre_Parse.Command |> Ok
        | Error e -> Error e
    | _ -> error "match_command_3" "Unexpected command." ["command", name] |> invalidOp

(* Main functions - matching *)

let match_commands
    (backgrounds : Map<string, string>)
    (characters : Character_Input_Map)
    (music_tracks : Map<string, string>)
    (scenes : Script list)
(* This is for error reporting. *)
    (scene_name : string)
    (commands : (int * string) list)
    : Command_Pre_Parse list =

    let get_command_and_parameters (text : string) : string * string =
        let i = text.IndexOf ' '
        if -1 = i then text, System.String.Empty
        else text.[0..i - 1].Trim (), text.[i..].Trim ()

    let rec helper
        (command_acc : Command_Pre_Parse list)
        (errors_acc : Parser_Error list)
        (commands : (int * string) list)
        : (Command_Pre_Parse list) * (Parser_Error list) =

        match commands with

(* We append each command and error to its respective accumulator, so reverse the accumulators before returning them. *)
        | [] -> command_acc |> List.rev, errors_acc |> List.rev

        | (line_number, line) :: tail ->

            #if debug
            debug "match_commands" "Trying to match command." ["command", line]
            #endif

            match match_dialogue line characters with
            | Some result -> helper (result :: command_acc) errors_acc tail
            | None ->

                let command, parameters = get_command_and_parameters line

(* TODO1 #parsing Remove double matching on menu start, image map start, image map end.
Again, we want to re-use the match command/parameters code, so we can for instance get better error reporting.
*)

(* Discard single-line comments. *)
                if command |> single_line_comment_regex.IsMatch then
                    helper command_acc errors_acc tail
(* Discard multi-line comments. *)
                elif command |> multi_line_comment_start_regex.IsMatch then
                    let remaining_commands = collect_multi_line_comment tail
                    helper command_acc errors_acc remaining_commands
(* If we encounter a javascript block, collect the statements in the block. *)
                elif command |> javascript_start_regex.IsMatch && String.IsNullOrEmpty parameters then
                    let result = collect_javascript [] tail
                    helper (Command_Pre_Parse.Command result.command :: command_acc) errors_acc result.remaining_commands
(* If we encounter a menu block, collect the statements in the block. *)
                elif command |> menu_start_regex.IsMatch then
                    let menu_data = (match_menu_start line).Value
                    let result = collect_menu menu_data tail
                    helper (Command_Pre_Parse.Menu result.menu_data :: command_acc) errors_acc result.remaining_commands
(* If we encounter an image map block, collect the statements in the block. *)
                elif command |> image_map_start_regex.IsMatch then
                    let image_map_data = (match_image_map_start line backgrounds).Value
                    let result = collect_image_map image_map_data tail
                    helper (Command_Pre_Parse.Image_Map result.image_map_data :: command_acc) errors_acc result.remaining_commands
(* We handle end_image_map separately and use it to fade out the image map. *)
                elif command |> end_image_map_regex.IsMatch then 
                    helper (Command_Pre_Parse.End_Image_Map (match_image_map_end line).Value :: command_acc) errors_acc tail
(* Otherwise, determine what kind of command this is. *)
                else
                    let result_1 =
                        match match_command_and_parameters command_patterns_2 line_number command parameters with
                        | Error e -> Error e
                        | Ok (name, parameters) ->
                            matched_command_to_pre_parse_command name parameters scenes music_tracks backgrounds characters
                    match result_1 with
                    | Ok result_2 -> helper (result_2 :: command_acc) errors_acc tail
                    | Error e -> helper command_acc (e :: errors_acc) tail

    let command_acc, errors_acc = commands |> helper [] []
    if List.isEmpty errors_acc then command_acc
    else
        error "match_commands" "Failed to match one or more commands in a scene script." [
            "scene_name", scene_name
            "errors", errors_acc
        ] |> invalidOp
