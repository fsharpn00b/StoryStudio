module Plugins

open Fable.Core.JsInterop
open Feliz

let private ensure_plugin_registry () =
    if isNull (Browser.Dom.window?Plugins) then
        Browser.Dom.window?Plugins <- createObj []

let private load_script (url: string) =
    promise {
        let script = Browser.Dom.document.createElement ("script")
        script?src <- url
        script?``type`` <- "module"

        let! _ =
            Promise.create(fun resolve reject ->
                script?onload <- resolve
                script?onerror <- reject
                Browser.Dom.document.body.appendChild (script) |> ignore
            )

        return ()
    }

let DynamicComponentLoader<'Interface> (interface_ref : IRefValue<'Interface>) (path : string) =
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

    if not is_loaded then
        Html.div "Loading..."
    else
        let component_ = Browser.Dom.window?Plugins?Inventory
        Feliz.Interop.reactApi.createElement(component_, {| expose = interface_ref |})
