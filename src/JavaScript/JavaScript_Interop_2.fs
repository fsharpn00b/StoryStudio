module JavaScript_Interop_2

// String.Empty
open System

// console, window
open Browser.Dom
// jsNative
open Fable.Core

open JavaScript_Interop_1
open Log
open Scripts

(* Debug *)

let debug_module_name = "JavaScript_Interop_2"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

(* We need to copy the state by value, not by reference. *)
let get_state_from_js () : obj = eval_js_with_exception $"JSON.parse(JSON.stringify(window.{game_state_name}));"

(* See also JavaScript_Interop_1.eval_js ()/eval_js_with_exception (). *)
[<Emit(set_state_in_js_emit)>]
let private set_state_in_js (state : obj) : unit = jsNative

let set_state_in_js_with_exception
    (state : obj)
    : unit =

    try set_state_in_js state with
    | e -> error "set_state_in_js" "Failed to set state." ["message", e.Message; "state", state] |> invalidOp

(* This is for debugging. *)
let show_js_state () : unit =
    let code = $"window.{game_state_name};"
    try
        console.log "JavaScript state:"
        console.log (eval_js_with_exception code)
(* An error here is not critical and should not stop the application. *)
    with e -> warn "show_js_state" true "Failed to get JavaScript state." ["message", e.Message; "code", code]
