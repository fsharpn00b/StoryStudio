module JavaScript_Interop

// String.Empty
open System

// console
open Browser.Dom
// jsNative
open Fable.Core

open Log
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "JavaScript_Interop"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

type private TypeScript_Compile_Result = { js_code : string; errors : string [] }

(* TODO1 #future Make it so value can be any type. Including discriminated unions.
Replace this with a JavaScript_Environment type to which the author can add arbitrary variable definitions.
*)
type Menu_Variables = Map<string, int>

(* Functions - JavaScript *)

[<Emit("eval($0)")>]
let private eval_js (code : string) : obj = jsNative

[<Emit("eval($0)")>]
let private eval_js_boolean (code : string) : bool =
    let result = jsNative code
    unbox<bool> result

[<Emit("eval($0)")>]
let private eval_js_string (code : string) : string =
    let result = jsNative code
    unbox<string> result

(* We need to copy the state by value, not by reference. *)
let get_state_from_js () : obj = eval_js $"JSON.parse(JSON.stringify(window.{game_state_name}));"

[<Emit(set_state_in_js_emit)>]
let set_state_in_js (code : obj) : unit = jsNative

let private emit_menu_variables (menu_variables : Menu_Variables) : string =
    (String.Empty, menu_variables) ||> Seq.fold (fun acc kv ->
        $"{acc}var {kv.Key} = {kv.Value};{Environment.NewLine}"
    )

let eval_js_with_menu_variables (code : string) (menu_variables : Menu_Variables) =
    eval_js $"{emit_menu_variables menu_variables}{code}"

let eval_js_boolean_with_menu_variables (code : string) (menu_variables : Menu_Variables) =
    eval_js_boolean $"{emit_menu_variables menu_variables}{code}"

let eval_js_string_with_menu_variables (code : string) (menu_variables : Menu_Variables) =
    eval_js_string $"{emit_menu_variables menu_variables}{code}"

(* In some cases we must run JavaScript code that might fail. If the JavaScript code fails, eval_js_string returns null, which is a valid value of System.String. We check for that here and return String.Empty instead. *)
let try_eval_js_string_with_menu_variables (code : string) (menu_variables : Menu_Variables) =
    let result = eval_js_string $"{emit_menu_variables menu_variables}{code}"
    if isNull result then String.Empty
    else result        

(* This is for debugging. *)
let show_js_state () : unit =
    do
        console.log "JavaScript state:"
        console.log (eval_js $"window.{game_state_name};")
// This also works.
//    do console.log (eval_js <| $"window.{game_state_name};")

(* Functions - TypeScript *)

(* We do not use these for now. *)
(*
[<Emit("transpile_ts($0)")>]
let transpile_ts (code: string) : string = jsNative

[<Emit("transpile_ts_with_check($0)")>]
let private transpile_ts_with_check (code: string) : obj = jsNative

let private check_ts (ts_code: string) : string =
    let result = ts_code |> transpile_ts_with_check |> unbox<TypeScript_Compile_Result>
    if result.errors.Length > 0 then
        error "check_ts" "TypeScript compile errors" [ "errors", result.errors |> String.concat Environment.NewLine :> obj ] |> invalidOp
    else result.js_code

let eval_ts (ts_code : string) : obj =
    let js_code = check_ts ts_code
    eval_js js_code

let eval_ts_boolean (ts_code : string) : bool =
    let js_code = check_ts ts_code
    eval_js_boolean js_code

let eval_ts_string (ts_code : string) : string =
    let js_code = check_ts ts_code
    eval_js_string js_code
*)
