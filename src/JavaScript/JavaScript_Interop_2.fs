module JavaScript_Interop_2

// String.Empty
open System

// console, window
open Browser.Dom
// jsNative
open Fable.Core

open JavaScript_Interop_1
open Log
open Save_Load_Validation
open Scripts

(* Debug *)

let debug_module_name = "JavaScript_Interop_2"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

(* Persist JavaScript state as JSON text. *)
let get_javascript_state_json () : string =
    try
        eval_js_with_exception $"JSON.stringify(window.{javascript_state_name} || {{}});" |> unbox<string>
    with e ->
        error
            "get_javascript_state"
            $"Failed to serialize JavaScript state. window.{javascript_state_name} must be JSON-serializable for save/load/undo/redo."
            ["error_message", e.Message]
            |> invalidOp

(* See also JavaScript_Interop_1.eval_js ()/eval_js_with_exception (). *)
[<Emit(set_javascript_state_emit)>]
let private set_javascript_state (javascript_state_json : string) : unit = jsNative

(* This is called by:
1. Runner_UI.run (), which just sets it to empty ("{}").
2. Runner_State.set_state (), which is called by:
2a. Runner_Save_Load.load_game ()
2b. Runner_History.undo_redo ().

Runner_Save_Load.load_game () is called on receipt of the Message_Load_Game message. That is dispatched by:
2a1. Save_Load_Storage.import_current_game ().
2a2. Save_Load_Rendering.handle_slot_click () when the player clicks on a saved game slot in Load_Game mode.

In both cases, we validate the JavaScript state beforehand.
For (2a1), we call Save_Load_Storage.validate_saved_game () -> Save_Load_Validation.validate_and_parse_runner_saveable_state () -> validate_runner_saveable_state () -> validate_javascript_state ().
For (2a2), we call Save_Load_Validation.validate_and_parse_runner_saveable_state () -> ....

If we fail to validate the JavaScript state, we warn and abort loading the game. So for (2a), it is reasonable to error if the JavaScript state is invalid, because we should never reach that point.

For (2b), we error because we have already discovered the game is an inconsistent state (the undo/redo history is corrupted).
*)
let set_javascript_state_with_exception
    (javascript_state_json : string)
    : unit =

(* See previous comment for why we validate here as well as when we load a saved game.*)
    match validate_javascript_state javascript_state_json with
    | Error (message, data) ->
        error "set_javascript_state_with_exception" "Failed to validate JavaScript state." (["javascript_state_json", javascript_state_json; "error_message", message] @ data) |> invalidOp
    | Ok () ->
        try
            set_javascript_state javascript_state_json
        with e ->
            error "set_javascript_state_with_exception" "Failed to set JavaScript state." ["error_message", e.Message; "javascript_state_json", javascript_state_json] |> invalidOp

(* This is for debugging. *)
let show_javascript_state () : unit =
    let code = $"window.{javascript_state_name};"
    try
        console.log "JavaScript state:"
        console.log (eval_js_with_exception code)
(* An error here is not critical and should not stop the application. *)
    with e -> warn "show_javascript_state" true "Failed to get JavaScript state." ["error_message", e.Message; "code", code]
