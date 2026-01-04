module Utilities

open System.Collections.Generic
open System.Text.RegularExpressions

open Units_Of_Measure

(* Consts*)

let pre_transition_time = 50<milliseconds>

let notify_transition_complete_delay_time = 10<milliseconds>

let wait_for_transitions_to_complete_time = 10<milliseconds>

let hide_save_load_screen_delay_time = 10<milliseconds>

let character_z_index = 1
let dialogue_box_z_index = 2
let dialogue_box_character_name_z_index = 3
let image_map_z_index = 4
let menu_z_index = 4
let image_map_hotspot_z_index = 5
let notifications_z_index = 6
let configuration_z_index = 7
let save_load_screen_z_index = 7
let command_menu_z_index = 8

let plugins_registry_name = "Plugins"
let interface_registry_name = "Interfaces"

(* Interfaces *)

type I_Transitionable =
    abstract member is_running_transition : unit -> bool
    abstract member force_complete_transition : unit -> unit
(* This is for debugging. *)
    abstract member get_name : unit -> string

(* Functions *)

let replace_non_alphanumeric_with_underscore (input : string) : string =
    Regex.Replace(input, "[^a-zA-Z0-9]", "_")

let duplicates_by (f : 'a -> 'b) (xs : 'a list) : ('b * 'a) list =
    xs
        |> List.groupBy f
        |> List.filter (fun (_, values) -> values.Length > 1)
        |> List.collect (fun (key, values) ->
            values |> List.map (fun value -> key, value)
        )
