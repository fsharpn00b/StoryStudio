module Log

// Environment.NewLine
open System

// console, window
open Browser.Dom

open Utilities

(* We use these custom exception handling functions instead of .NET exceptions because:
1 In Fable, .NET exceptions do not support the Data field.
2 .NET exceptions appear in the browser console log, but do not alert the user.
3 .NET exceptions show the stack trace in terms of our generated *.fs.js files, but that is not helpful to the user. Instead, they need to know which of their scripts, and which line in that script, caused the error.
(end)

When we catch a .NET exception, we extract the message.
*)

(* Types *)

(* TODO1 #logging Replace (string * obj) list with Error_Data everywhere.

- Also consider types:
type Warning_Data = {
    module_name : string
    function_name : string
    alert : bool
    message : string
    data : Error_Data
}

type Error_Data = {
    module_name : string
    function_name : string
    message : string
    data : (string * obj) list
}
*)
type Error_Data = (string * obj) list

type log_function = string -> string -> Error_Data -> unit
type warn_function = string -> bool -> string -> Error_Data -> unit 
type error_function = string -> string -> Error_Data -> string

type private Severity =
    | Debug
    | Warning
    | Error

(* Functions *)

let private log
    (module_name : string) 
    (function_name : string) 
    (severity_1 : Severity)
    (message : string)
    (data_1 : Error_Data)
    : string =
    let severity_2 =
        match severity_1 with
        | Debug -> "DEBUG: "
        | Warning -> "WARNING: "
        | Error -> "ERROR: "

(* When calling log (), be sure to convert any sequences in data_1 to lists before upcasting them to objects, or else they will show as "{}". *)
    let data_2 = (String.Empty, data_1) ||> List.fold (fun acc (name, value) ->
        $"{acc}{name}:{Environment.NewLine}{json_stringify value}{Environment.NewLine}"
    )

    $"{severity_2}{module_name}.{function_name}: {message}{Environment.NewLine}{data_2}"

let debug (module_name : string) (function_name : string) (message : string) (data : Error_Data) : unit =
    do console.log (log module_name function_name Debug message data)

let warn (module_name : string) (function_name : string) (alert : bool) (message : string) (data : Error_Data) =
    if alert then
        do window.alert $"{message}{Environment.NewLine}See browser console for more information."
    do console.log (log module_name function_name Warning message data)

let error (module_name : string) (function_name : string) (message : string) (data : Error_Data) : string =
(* Given that an error stops the program, we always alert the player. *)
    do window.alert $"{message}{Environment.NewLine}See browser console for more information."
    log module_name function_name Error message data

(* We do not use this for now. *)
(* This makes the error () function accessible from JavaScript. *)
(*
do window?report_error <- error
*)
