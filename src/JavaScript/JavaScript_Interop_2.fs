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

(* TODO1 #javascript This still handles the exception in the emitted code and uses window?report_error ()/Log.error (). If we continue to do this, add a note. 

- Also, using try/with with jsNative does not work. We need to remove that but preserve whatever information is there, such as that we were calling set_state_in_js ().

- Can we use eval_js_with_exception ()? That raises Run_Time_JavaScript_Error, which is meant to be caught and converted to Log.error ().

- Maybe eval_js () should have two paths:
1 eval_js_with_exception (), which is used by commands, and raises an exception to be caught by Runner_Queue.run_command ().
2 eval_js for get and set state, which handles the exception in the emitted code and uses window?report_error ()/Log.error ().
(end)

- Why does set_state_in_js even take a parameter? The code to set state should be a constant/literal?

*)
[<Emit(set_state_in_js_emit)>]
let set_state_in_js (code : obj) : unit =
    try jsNative
    with exn -> error "set_state_in_js" exn.Message ["code", code] |> invalidOp

(* This is for debugging. *)
let show_js_state () : unit =
    let code = $"window.{game_state_name};"
    try
        console.log "JavaScript state:"
        console.log (eval_js_with_exception code)
(* An error here is not critical and should not stop the application. *)
    with e -> warn "show_js_state" true "Failed to get JavaScript state." ["code", code; "message", e.Message]
