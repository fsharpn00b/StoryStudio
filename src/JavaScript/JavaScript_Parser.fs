module JavaScript_Parser

// Environment.NewLine, String
open System

// console, window
open Browser.Dom

open Command_Types
open Image_Map
open Menu
open Log
open Save_Load_Storage_Helpers
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "JavaScript_Parser"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Consts *)

(* initial_javascript initializes window.state in a way that is appropriate for the application but not for the environment we use to run javascript_for_checking. *)

(* Types *)

type private Parser_JavaScript_Path_Accumulator = {
    javascript : string list
    scenes_encountered : Set<int<scene_id>>
    paths_to_try : Parser_JavaScript_Path list
    encountered_if : bool
}
and private Parser_JavaScript_Path = {
    scene_id : int<scene_id>
    command_id : int<command_id> option
    accumulator : Parser_JavaScript_Path_Accumulator
}

type private Parser_JavaScript_Accumulator = {
    paths_tried : Parser_JavaScript_Path_Accumulator list
    paths_to_try : Parser_JavaScript_Path list
}

(* Functions - helper *)

let private enclose_javascript_in_function (javascript : string) : string = @$"
(function () {{
    {javascript}
}}) ();
"

let private combine_javascript_interpolations (javascript_interpolations : string list) : string =
    javascript_interpolations |> List.map (fun x -> $"console.log({x});{Environment.NewLine}") |> String.concat String.Empty

let private combine_javascript_conditionals (javascript_conditionals : string list) : string =
    javascript_conditionals |> List.map (fun x -> $"if ({x}) {{}}{Environment.NewLine}") |> String.concat String.Empty

let private handle_command_javascript (command_1 : Command) : string option =
    match command_1 with
    | JavaScript_Inline command_2
    | JavaScript_Block command_2 -> Some $"{command_2}{Environment.NewLine}"
    | Dialogue command_2 -> combine_javascript_interpolations command_2.javascript_interpolations |> Some
    | Temporary_Notification command_2 -> combine_javascript_interpolations command_2.javascript_interpolations |> Some
    | Permanent_Notification command_2 -> combine_javascript_interpolations command_2.javascript_interpolations |> Some
    | _ -> None

let private handle_menu_javascript (menu : Menu_Data_1) : string =
    String.concat String.Empty [
        combine_javascript_interpolations menu.javascript_interpolations
        combine_javascript_interpolations (menu.items |> List.collect (fun menu_item -> menu_item.javascript_interpolations))
        combine_javascript_conditionals (menu.items |> List.choose (fun menu_item -> menu_item.conditional))
        $"var {menu.name} = 1;{Environment.NewLine}"
    ]

let private handle_image_map_javascript (image_map : Image_Map_Data) : string =
    String.concat String.Empty [
        combine_javascript_interpolations (image_map.items |> List.collect (fun image_map_item -> image_map_item.javascript_interpolations))
        combine_javascript_conditionals (image_map.items |> List.choose (fun image_map_item -> image_map_item.conditional))
        $"var {image_map.name} = 1;{Environment.NewLine}"
    ]

let private handle_if_javascript (acc : Parser_JavaScript_Path_Accumulator) (scene_id : int<scene_id>) (if_block : If_Block) =
    let javascript_1 =
        [
            [ $"if ({if_block.conditional}) {{}}{Environment.NewLine}" ]
            if_block.else_if_blocks |> List.map (fun else_if_block -> $"if ({else_if_block.conditional}) {{}}{Environment.NewLine}")
        ] |> List.concat
    let javascript_2 = String.concat Environment.NewLine javascript_1

    let new_accumulator = {
        acc with
            paths_to_try = []
            javascript = javascript_2 :: acc.javascript
    }
    let paths_to_try =
        [
            [{ scene_id = scene_id; command_id = Some if_block.child_command_id; accumulator = new_accumulator }]

            if_block.else_if_blocks |> List.map (fun else_if_block ->
                { scene_id = scene_id; command_id = Some else_if_block.child_command_id; accumulator = new_accumulator }
            )

            match if_block.else_block with
            | Some child_command_id ->
                [{ scene_id = scene_id; command_id = Some child_command_id; accumulator = new_accumulator }]
            | None -> []
        ] |> List.concat

    { acc with encountered_if = true; paths_to_try = paths_to_try @ acc.paths_to_try }

(* Functions - main *)

(* 20251105 We verified
- initial_javascript is added to the entry script (start.txt).
- initial_javascript is necessary. Without it, setting window.state.* values fails.
- The commands we run in javascript_for_checking (such as setting window.state.* values) do not contaminate the JavaScript environment of the application.
(end)
*)

let rec private try_javascript_path
    (acc : Parser_JavaScript_Path_Accumulator)
    (scenes : Scene_Map)
    (scene_id : int<scene_id>)
    (scene : Scene)
    (command_id_1 : int<command_id> option)
    : Parser_JavaScript_Path_Accumulator =
    match command_id_1 with

    | None -> acc

    | Some command_id_2 ->

        let command = scene.[command_id_2]
        match command.command with

        | Command command_2 ->
            match command_2 with

            | Jump destination ->
                if acc.scenes_encountered.Contains destination then acc
                else
                    try_javascript_path {
                        acc with
                            javascript = $"// {destination}{Environment.NewLine}" :: acc.javascript
                            scenes_encountered = acc.scenes_encountered.Add destination
                    } scenes destination scenes.[destination] <| Some scene_initial_command_id

            | _ ->
                let javascript_1 =
                    match handle_command_javascript command_2 with
                    | Some javascript_2 -> javascript_2 :: acc.javascript
                    | None -> acc.javascript
                try_javascript_path { acc with javascript = javascript_1 } scenes scene_id scene command.next_command_id

        | Menu menu ->
            try_javascript_path {
                acc with javascript = (handle_menu_javascript menu) :: acc.javascript
            } scenes scene_id scene command.next_command_id

        | Image_Map image_map ->
            try_javascript_path {
                acc with javascript = (handle_image_map_javascript image_map) :: acc.javascript
            } scenes scene_id scene command.next_command_id

        | End_Image_Map _ -> try_javascript_path acc scenes scene_id scene command.next_command_id

        | If if_block -> handle_if_javascript acc scene_id if_block

        | End_If -> try_javascript_path acc scenes scene_id scene command.next_command_id

let rec private try_javascript_paths
    (acc : Parser_JavaScript_Accumulator)
    (scenes : Scene_Map)
    : Parser_JavaScript_Accumulator =
    match acc.paths_to_try with
    | [] -> acc
    | head :: tail ->
        let path_accumulator = try_javascript_path head.accumulator scenes head.scene_id scenes.[head.scene_id] head.command_id
        try_javascript_paths {
            acc with
                paths_tried =
                    if path_accumulator.encountered_if then acc.paths_tried
                    else { path_accumulator with javascript = path_accumulator.javascript |> List.rev } :: acc.paths_tried
                paths_to_try = path_accumulator.paths_to_try @ tail
        } scenes

let private get_javascript_paths (scenes : Scene_Map) : string list =
    let initial_acc = {
        paths_tried = []
        paths_to_try = [{
            scene_id = entry_scene_id
            command_id = Some scene_initial_command_id
            accumulator = { 
                javascript = []
                scenes_encountered = Set.singleton entry_scene_id
                paths_to_try = []
                encountered_if = false
            }
        }]
    }
    let results = try_javascript_paths initial_acc scenes
    results.paths_tried |> List.map (fun path -> path.javascript |> String.concat String.Empty)

(* TODO1 #javascript Again, Scene_Map doesn't give us individual scene/script names, so we can't include that in the output.
This argues in favor of expanding Scene/Scene map to include script name/contents.
*)
let check_javascript (scenes : Scene_Map) : unit =
    let javascript_1 =
        scenes
        |> get_javascript_paths
        |> List.map enclose_javascript_in_function
        |> String.concat Environment.NewLine
    let file_name = $"{get_current_timestamp ()}.ts"
    let javascript_2 =
        $"""
/*
To validate this file, run
npx tsc --noEmit --strict {file_name}
(end)
If you saved it with a different name than the default, make sure it has a .ts extension.
To install TypeScript, run
npm install typescript --save-dev
(end)
The --save-dev means you are installing the package only for dev purposes, not to be deployed with the application.
*/

{get_typescript_types ()}

{javascript_1}
"""
    download_file file_name "application/x-typescript" javascript_2
