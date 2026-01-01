module Plugins

// jsNative
open Fable.Core
// ? operator, createObj
open Fable.Core.JsInterop
// React
open Feliz
// Decode
open Thoth.Json

open Log

(* Types *)

type Plugin_Data = {
    component_ : ReactElement
// TODO1 #dynamic_load Explain the obj must be cast to the interface later.
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
    if isNull (Browser.Dom.window?Plugins) then
        Browser.Dom.window?Plugins <- createObj []

(* Functions - main *)

let private load_script (path : string) =
    promise {
        let script = Browser.Dom.document.createElement "script"
        script?src <- path
        script?``type`` <- "module"

        let! _ =
            Promise.create(fun resolve reject ->
                script?onload <- resolve
                script?onerror <- reject
                Browser.Dom.document.body.appendChild script |> ignore
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
        let component_ = Browser.Dom.window?Plugins?(name)
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
