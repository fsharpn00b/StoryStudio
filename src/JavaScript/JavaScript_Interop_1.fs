module JavaScript_Interop_1

// String.Empty
open System

// console, window
open Browser.Dom
// jsNative
open Fable.Core
// ? operator
open Fable.Core.JsInterop

open Log

(* Debug *)

let debug_module_name = "JavaScript_Interop_1"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

(* TODO1 #javascript #future Make it so value can be any type. Including discriminated unions.
Replace this with a JavaScript_Environment type to which the author can add arbitrary variable definitions.
*)
type Menu_Variables = Map<string, int>

(* We cannot include script_text_index here because it is not available to eval_js_with_exception (), which can be called for non-command reasons:
1 Getting the JavaScript state as part of getting the game state to create a snapshot.
2 Getting the JavaScript state for debugging.
(end)

Instead, we get script_text_index for each command in Parser_1_Semantics. Then, in Runner_Queue.run_command (), we check for this exception type and add the script name and calculate the line number that caused the error.
*)
type Run_Time_JavaScript_Error_Data = {
    code : string
    inner : exn
}
exception Run_Time_JavaScript_Error of Run_Time_JavaScript_Error_Data

(* Functions - JavaScript *)

(* eval_js () can throw an exception, which must be handled by the caller. The only way to handle it here is in the emit attribute, which does not let us use our custom F# exception types. *)
[<Emit("eval($0)")>]
let private eval_js (code : string) : obj = jsNative

(* This raises an exception, rather than call Log.error (), because we still need to get the script name and line number that caused the error. In Runner_Queue.run_command (), we check for this exception type. We use a custom exception type because Fable does not support the .NET exception Data field. *)
let eval_js_with_exception (code : string) : obj =
    try eval_js code with
    | e ->
        {
            code = code
            inner = e
        } |> Run_Time_JavaScript_Error |> raise

let private emit_menu_variables (menu_variables : Menu_Variables) : string =
    (String.Empty, menu_variables) ||> Seq.fold (fun acc kv ->
        $"{acc}var {kv.Key} = {kv.Value};{Environment.NewLine}"
    )

(* TODO1 #javascript Re-check everywhere we can use conditionals and make sure they're included in check_javascript. We don't think notifications are, for instance? No, they are. Just check all callers of this function and make a list.

- Also, test error handling for every command that can use JavaScript.

1 js
js console.log (x);
(end)
2 js/endjs
js
console.log (x);
endjs
(end)
*)
let eval_js_with_menu_variables<'T> (code : string) (menu_variables : Menu_Variables) : 'T =
    eval_js_with_exception $"{emit_menu_variables menu_variables}{code}" |> unbox<'T>

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
