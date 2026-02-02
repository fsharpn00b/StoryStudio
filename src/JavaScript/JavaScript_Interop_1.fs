module JavaScript_Interop_1

// String.Empty
open System

// console, window
open Browser.Dom
// jsNative
open Fable.Core

open Log

(* Debug *)

let debug_module_name = "JavaScript_Interop_1"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* TODO1 #javascript #future Make it so value can be any type. Including discriminated unions.
Replace this with a JavaScript_Environment type to which the author can add arbitrary variable definitions.
*)
type Menu_Variables = Map<string, int>

(* Functions - JavaScript *)

(* Notes
- window.report_error is the error () function defined in Log, not the version we have defined here that is closed over the module name (JavaScript_Interop_1), so we still have to provide that.
- We cannot get Fable to emit an F# tuple to JavaScript correctly, so we must leave the data parameter to window.report_error ()/Log.error () empty.
*)

// TODO1 #javascript Use this again if we move error handling higher up the call chain.
(*
[<Emit("eval($0)")>]
*)

[<Emit("""
(function() {
    try {
        return eval($0);
    } catch (exn) {
        throw (window.report_error("JavaScript_Interop_1", "eval_js", `\nJavaScript error:\n${exn.message}\nCode:\n${$0}`, []));
    }
})()
""")>]
let eval_js (code : string) : obj = jsNative

let private emit_menu_variables (menu_variables : Menu_Variables) : string =
    (String.Empty, menu_variables) ||> Seq.fold (fun acc kv ->
        $"{acc}var {kv.Key} = {kv.Value};{Environment.NewLine}"
    )

(* TODO1 #javascript Instead of code, take JavaScript_Data? Then catch exception from eval_js?

- Problem, this is also called when evaluating conditionals. Which means if, else if, etc need to store their script text index as well.

- Also, re-check everywhere we can use conditionals and make sure they're included in check_javascript. We don't think notifications are, for instance? No, they are. Just check all callers of this function and make a list.

- This is probably the right place to catch exceptions from eval_js, add data such as line number, then rethrow.
*)
let eval_js_with_menu_variables<'T> (code : string) (menu_variables : Menu_Variables) : 'T =
    eval_js $"{emit_menu_variables menu_variables}{code}" |> unbox<'T>

(* In some cases we must run JavaScript code that might fail. If the JavaScript code fails, eval_js_string returns null, which is a valid value of System.String. We check for that here and return String.Empty instead. *)
(* We do not use this for now.
TODO2 #javascript This will no longer work with the new exception handling in eval_js.
*)
(*
let try_eval_js_string_with_menu_variables (code : string) (menu_variables : Menu_Variables) : string option =
    let result = eval_js_string $"{emit_menu_variables menu_variables}{code}"
    if isNull result then None
    else Some result
*)

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
