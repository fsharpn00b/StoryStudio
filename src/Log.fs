module Log

// Environment.NewLine
open System
// StringBuilder
open System.Text

// console
open Browser.Dom

// Encode
open Thoth.Json

type log_function = string -> string -> (string * obj) list -> unit
type warn_function = string -> bool -> string -> (string * obj) list -> unit 
type error_function = string -> string -> (string * obj) list -> string

type private Severity =
    | Debug
    | Warning
    | Error

(* This is to prevent objects from being printed as "[object Object]". *)
let inline private json_stringify (o : 'a) : string = Encode.Auto.toString (2, o)

let private log
    (module_name : string) 
    (function_name : string) 
    (severity_1 : Severity)
    (message : string)
    (data_1 : (string * obj) list)
    : string =
    let severity_2 =
        match severity_1 with
        | Debug -> "DEBUG: "
        | Warning -> "WARNING: "
        | Error -> "ERROR: "

(* When calling log (), be sure to convert any sequences in data_1 to lists before upcasting them to objects, or else they will show as "{}". *)
    let data_2 = (new StringBuilder (), data_1) ||> List.fold (fun acc item ->
        acc.AppendLine (value = $"{fst item}:{Environment.NewLine}{item |> snd |> json_stringify}")
    )

    $"{severity_2}{module_name}.{function_name}: {message}{Environment.NewLine}{data_2}"

let debug (module_name : string) (function_name : string) (message : string) (data : (string * obj) list) : unit =
    do console.log (log module_name function_name Debug message data)

let warn (module_name : string) (function_name : string) (alert : bool) (message : string) (data : (string * obj) list) =
    if alert then
        do window.alert $"{message}{Environment.NewLine}See browser console for more information."
    do console.log (log module_name function_name Warning message data)

let error (module_name : string) (function_name : string) (message : string) (data : (string * obj) list) : string =
    log module_name function_name Error message data
