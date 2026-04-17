module Plugins

// Exception
open System

// console, document, window
open Browser.Dom
// jsNative
open Fable.Core
// ? operator, createObj
open Fable.Core.JsInterop
// React
open Feliz
// Decode
open Thoth.Json

open Log
open Runner_Types_2
open Utilities

(* TODO1 #plugins Allow author-configurable plugin hotkeys for players to interact with plugins (for example, press "i" for inventory). Each plugin could export a list of hotkeys tied to functions it defines. Have to handle conflict between that and built-in hotkeys though. Also need to import the plugin hotkeys into our configuration screen. Maybe just let plugin specify default hotkeys, then if they conflict with built-in hotkeys, assign them to the next available letter and let the player configure it. Not very satisfactory.
*)

(* Debug *)

let debug_module_name = "Plugins"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

type private Plugin_Manifest_Entry = {
    name : string
    path : string
}

(* Consts *)

[<Literal>]
let plugins_path = "../0_data/plugins/"
[<Literal>]
let plugins_configuration_path = "../0_data/plugins.txt?raw"

[<ImportDefault(plugins_configuration_path)>]
let private plugins_1 : string = jsNative

(* Functions - helper *)

let private is_valid_plugin_path (path : string) : bool =
    not (String.IsNullOrWhiteSpace path) &&
        path.StartsWith(plugins_path) &&
        path.EndsWith(".js") &&
        not (path.Contains("/../")) &&
        not (path.Contains("..\\")) &&
        not (path.Contains("?")) &&
        not (path.Contains("#"))

let private validate_plugin_entries (plugins_2 : Plugin_Manifest_Entry list) : unit =
    let invalid_names =
        plugins_2
            |> List.filter (fun entry -> not <| is_valid_name entry.name)
            |> List.map (fun entry -> entry.name)
    let invalid_paths =
        plugins_2
            |> List.filter (fun entry -> not <| is_valid_plugin_path entry.path)
            |> List.map (fun entry -> $"{entry.name}: {entry.path}")

    if not <| List.isEmpty invalid_names then
        error
            "validate_plugin_entries"
            $"Plugin names must be non-empty and use only valid characters: {valid_name_characters}."
            ["invalid_plugin_names", String.concat ", " invalid_names]
            |> invalidOp

    if not <| List.isEmpty invalid_paths then
        error
            "validate_plugin_entries"
            "Plugin paths must start with '../0_data/plugins/', end with '.js', and not use traversal, query, or fragment syntax."
            ["invalid_plugin_paths", String.concat ", " invalid_paths]
            |> invalidOp

    let duplicates = duplicates_by (fun entry -> entry.name) plugins_2
    if not <| List.isEmpty duplicates then
        let duplicate_names =
            duplicates
                |> List.map fst
                |> List.distinct
                |> String.concat ", "
        error "validate_plugin_entries" "Plugin names must be unique." ["duplicate_names", duplicate_names] |> invalidOp

let private get_plugin_paths () : Map<string, string> =
    match Decode.Auto.fromString<Plugin_Manifest_Entry list> plugins_1 with
    | Ok plugins_2 ->
        do validate_plugin_entries plugins_2
        plugins_2 |> List.map (fun entry -> entry.name, entry.path) |> Map.ofList
    | Error message -> error "get_plugin_paths" "Failed to deserialize plugins." ["plugins", plugins_1; "error_message", message] |> invalidOp

let private create_interface_ref () : IRefValue<obj> =
    createObj ["current" ==> null] |> unbox<IRefValue<obj>>

let private ensure_interface_registry () =
    if isNull (window?(interface_registry_name)) then
        window?(interface_registry_name) <- createObj []

let private ensure_plugin_registry () =
    if isNull (window?(plugins_registry_name)) then
        window?(plugins_registry_name) <- createObj []

(* We get all available plugin scripts for error reporting. See load_script (). *)
[<Emit("import.meta.glob('../0_data/plugins/*.js')")>]
let private vite_plugin_glob () : obj = jsNative

let private load_script (path : string) =
    promise {
        let loaders = vite_plugin_glob ()
        let loader_1 = loaders?(path)

        if isNull loader_1 then
            error "load_script" "Failed to find plugin in Vite module map." ["path", path; "available_paths", JS.Constructors.Object.keys(loaders)] |> invalidOp
        else
            let loader_2 = unbox<unit -> JS.Promise<obj>> loader_1
            let! _ = loader_2 ()
            return ()
    }

(* Functions - main *)

let get_plugins () : Plugins_Data =
    get_plugin_paths ()
        |> Seq.map (fun kv ->
            kv.Key, {
                path = kv.Value
                interface_ref = create_interface_ref ()
            }
        )
        |> Map.ofSeq

let emit_plugin_interfaces (plugins : Plugins_Data) : unit =
(* This prevents us from re-emitting the plugin interfaces on every render. *)
    React.useEffectOnce (fun () ->
        do
            ensure_interface_registry ()
            plugins |> Seq.iter (fun kv ->
(* We must emit a reference to the interface, rather than the interface itself, because the interface is null until Runner renders the component. *)
                window?(interface_registry_name)?(kv.Key) <- kv.Value.interface_ref
            )
    )

[<ReactComponent>]
let Plugin_Host
    (props : {| name : string; path : string; interface_ref : IRefValue<obj> |})
    : ReactElement =

    let is_loaded, set_is_loaded = React.useState false

    React.useEffect(
        (fun () ->
            let result = promise {
                ensure_plugin_registry ()
                do! load_script props.path
                set_is_loaded true
            }
//            do result |> Promise.catch (fun _ -> set_is_error true) |> ignore
(* TODO2 This does not stop the app from running, presumably because it is in a React.useEffect function. However, handling the error outside this function creates multiple alerts. For now this is the less painful solution. *)
            do result |> Promise.catch (fun _ -> error "load_script" "Failed to load plugin script." ["path", props.path] |> invalidOp) |> ignore
        ),
        [| box props.path |]
    )

// TODO2 This creates multiple alerts.
(*
    if is_error then error "load_script" "Failed to load plugin script." ["path", path] |> invalidOp
    elif is_loaded then
*)
    if is_loaded then
        let component_ = window?(plugins_registry_name)?(props.name)
        Feliz.Interop.reactApi.createElement(component_, {| expose = props.interface_ref |})
    else Html.none
