module Utilities

open System.Collections.Generic
open System.Text.RegularExpressions

open Units_Of_Measure

(* Consts*)

let pre_transition_time = 50<milliseconds>

let notify_transition_complete_delay_time = 10<milliseconds>

let wait_for_transitions_to_complete_time = 10<milliseconds>

let hide_save_load_screen_delay_time = 10<milliseconds>

(* Interfaces *)

type I_Transitionable =
    abstract member is_running_transition : unit -> bool
    abstract member force_complete_transition : unit -> unit
(* This is for debugging. *)
    abstract member get_name : unit -> string

(* Functions *)

let replace_non_alphanumeric_with_underscore (input : string) : string =
    Regex.Replace(input, "[^a-zA-Z0-9]", "_")

let duplicates_by (f : 'a -> 'b) (xs : 'a list) : 'b list =
    let already_seen = HashSet<_> ()
    let duplicates = HashSet<_> ()
    for x in xs do
        let key = f x
        if not (already_seen.Add key) then
            duplicates.Add key |> ignore
    duplicates |> Seq.toList
