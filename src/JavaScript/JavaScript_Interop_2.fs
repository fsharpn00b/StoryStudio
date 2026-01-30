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

type private TypeScript_Compile_Result = {
    js_code : string
    errors : string []
}

(* We need to copy the state by value, not by reference. *)
let get_state_from_js () : obj = eval_js $"JSON.parse(JSON.stringify(window.{game_state_name}));"

[<Emit(set_state_in_js_emit)>]
let set_state_in_js (code : obj) : unit =
    try jsNative
    with exn -> error "set_state_in_js" exn.Message ["code", code] |> invalidOp

(* This is for debugging. *)
let show_js_state () : unit =
    do
        console.log "JavaScript state:"
        console.log (eval_js $"window.{game_state_name};")
// This also works.
//    do console.log (eval_js <| $"window.{game_state_name};")
