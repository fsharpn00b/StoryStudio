module Plugins

// document, window
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
open Utilities

(* Types *)

type Plugin_Data = {
    component_ : ReactElement
(* TODO1 We must store the interface as an obj because each component exposes a different interface and we do not know how to statically determine the interface type. If we did, we are not sure how we would store different interface types in a single map. Presumably they would all need to inherit from a base interface type, but there are no base interface methods we require, so they might as well inherit from obj.
In any case, we do not use these interfaces inside the framework. Instead, they are used by JavaScript written by the author. JavaScript does not need to cast the obj to the interface type. It is enough to say, for example:
window.Interfaces.<interface>.current.<method>
(end)

- However, we should let a plugin author provide a TypeScript definition of the interface that we can then use to check the JavaScript code.
*)
    interface_ref : IRefValue<obj>
}

type Plugins_Data = Map<string, Plugin_Data>

(* Consts *)

[<Literal>]
let plugins_path = "../0_data/plugins.txt?raw"

[<ImportDefault(plugins_path)>]
let private plugins_1 : string = jsNative

(* Debug *)

let debug_module_name = "Plugins"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Functions - helper *)

let private get_plugin_paths () : Map<string, string> =
    match Decode.Auto.fromString<{| name : string; path : string |} list> plugins_1 with
    | Ok plugins_2 -> plugins_2 |> List.map (fun entry -> entry.name, entry.path) |> Map.ofList
    | _ -> error "get_plugins" "Failed to deserialize plugins." ["plugins", plugins_1] |> invalidOp

let private ensure_plugin_registry () =
    if isNull (window?(plugins_registry_name)) then
        window?(plugins_registry_name) <- createObj []

let private ensure_interface_registry () =
    if isNull (window?(interface_registry_name)) then
        window?(interface_registry_name) <- createObj []

(* Functions - main *)

let private load_script (path : string) =
    promise {
        let script = document.createElement "script"
        script?src <- path
        script?``type`` <- "module"

        let! _ =
            Promise.create(fun resolve reject ->
                script?onload <- resolve
                script?onerror <- reject
                document.body.appendChild script |> ignore
            )

        return ()
    }

let private Dynamic_Component_Loader (name : string) (path : string) (interface_ref : IRefValue<obj>) : ReactElement =
    let is_loaded, set_is_loaded = React.useState false

    React.useEffect(
        (fun () ->
            promise {
                ensure_plugin_registry ()
                do! load_script path
                set_is_loaded true
            }
            |> ignore
        ),
        [||]
    )

    if is_loaded then
        let component_ = window?(plugins_registry_name)?(name)
        Feliz.Interop.reactApi.createElement(component_, {| expose = interface_ref |})
    else Html.none

let get_plugins () : Plugins_Data =
    get_plugin_paths ()
        |> Seq.map (fun kv ->
            let interface_ref = React.useRef<obj> Unchecked.defaultof<_>
            let component_ = Dynamic_Component_Loader kv.Key kv.Value interface_ref
            kv.Key, { component_ = component_; interface_ref = interface_ref }
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
