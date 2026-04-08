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

(* Persist JavaScript state as JSON text. *)
let get_state_from_js () : string =
    try
        eval_js_with_exception $"JSON.stringify(window.{game_state_name} || {{}});" |> unbox<string>
    with e ->
        error
            "get_state_from_js"
            $"Failed to serialize JavaScript state. window.{game_state_name} must be JSON-serializable for save/load/undo/redo."
            ["message", e.Message]
            |> invalidOp

(* See also JavaScript_Interop_1.eval_js ()/eval_js_with_exception (). *)
[<Emit(set_state_in_js_emit)>]
let private set_state_in_js (state_json : string) : unit = jsNative

let set_state_in_js_with_exception
    (state_json : string)
    : unit =

    try set_state_in_js state_json with
    | e -> error "set_state_in_js" "Failed to set state." ["message", e.Message; "state_json", state_json] |> invalidOp

(* This is for debugging. *)
let show_js_state () : unit =
    let code = $"window.{game_state_name};"
    try
        console.log "JavaScript state:"
        console.log (eval_js_with_exception code)
(* An error here is not critical and should not stop the application. *)
    with e -> warn "show_js_state" true "Failed to get JavaScript state." ["message", e.Message; "code", code]
