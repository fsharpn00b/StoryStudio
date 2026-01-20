module Key_Bindings

// console, document, window
open Browser.Dom
// HTMLTextAreaElement
open Browser.Types
// ReactElement
open Feliz

open Log
open Utilities

(* Debug *)

let debug_module_name = "Key_Bindings"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

type private Key_Binding = {
    name : string
    display_name : string
    configuration_display_order : int
    default_key : string
}

type private User_Configured_Key_Binding = {
    key : string
    name : string
    display_name : string
}

type Key_To_Key_Binding_Name = Map<string, string>
type Key_Binding_Name_To_Key = Map<string, string>

type Key_Bindings_Configuration = {
    key_to_name : Key_To_Key_Binding_Name
    name_to_key : Key_Binding_Name_To_Key
}

(* Consts *)

let private permanent_key_bindings = [
    {
        name = "escape"
        display_name = "Open/close Configuration screen"
        configuration_display_order = 0
        default_key = "Escape"
    }
]

let permanent_key_to_key_binding_names : Key_To_Key_Binding_Name =
    permanent_key_bindings
        |> List.map (fun binding -> binding.default_key, binding.name)
        |> Map.ofList

let private default_key_bindings : Key_Binding list = [
    {
        name = "continue"
        display_name = "Continue"
        configuration_display_order = 0
        default_key = " "
    }
    {
        name = "undo"
        display_name = "Undo"
        configuration_display_order = 1
        default_key = "b"
    }
    {
        name = "redo"
        display_name = "Redo"
        configuration_display_order = 2
        default_key = "n"
    }
    {
        name = "save_game"
        display_name = "Open Save Game screen"
        configuration_display_order = 3
        default_key = "s"
    }
    {
        name = "load_game"
        display_name = "Open Load Game screen"
        configuration_display_order = 4
        default_key = "l"
    }
    {
        name = "delete_game"
        display_name = "Open Delete Game screen"
        configuration_display_order = 5
        default_key = "d"
    }
    {
        name = "quicksave"
        display_name = "Quick save"
        configuration_display_order = 6
        default_key = "q"
    }
    {
        name = "export_saved_games"
        display_name = "Export saved games to file"
        configuration_display_order = 7
        default_key = "e"
    }
    {
        name = "import_saved_games"
        display_name = "Import saved games from file"
        configuration_display_order = 8
        default_key = "i"
    }
    {
        name = "export_current_game"
        display_name = "Export current game to file"
        configuration_display_order = 9
        default_key = "x"
    }
    {
        name = "import_current_game"
        display_name = "Import current game from file"
        configuration_display_order = 10
        default_key = "f"
    }
    {
        name = "configuration"
        display_name = "Open Configuration screen"
        configuration_display_order = 11
        default_key = "c"
    }
    {
        name = "screenshot"
        display_name = "Get screenshot"
        configuration_display_order = 12
        default_key = "g"
    }
    {
        name = "ui"
        display_name = "Show/hide UI"
        configuration_display_order = 13
        default_key = "u"
    }
]

let private default_debug_key_bindings : Key_Binding list = [
    {
        name = "queue"
        display_name = "Show runner queue"
        configuration_display_order = 0
        default_key = "Q"
    }
    {
        name = "characters"
        display_name = "Show character data"
        configuration_display_order = 1
        default_key = "C"
    }
    {
        name = "background"
        display_name = "Show background data"
        configuration_display_order = 2
        default_key = "B"
    }
    {
        name = "javascript"
        display_name = "Check JavaScript"
        configuration_display_order = 3
        default_key = "J"
    }
    {
        name = "state"
        display_name = "Show state"
        configuration_display_order = 4
        default_key = "T"
    }
]

(* Functions - rendering *)

let private get_key_binding_element
    (key_bindings_configuration : Key_Bindings_Configuration)
    (binding : Key_Binding) : ReactElement seq =

    [
        Html.label [ prop.text $"{binding.display_name}: " ]
        Html.input [
            prop.id $"txt_{binding.name}"
            prop.type' "text"
            prop.maxLength 1
            prop.style [style.width (length.em 2)]
            prop.defaultValue (
                match key_bindings_configuration.name_to_key.TryFind binding.name with
                | Some key -> string key
                | None -> error "get_key_binding_elements" "Missing key binding." ["key binding name", binding.name; "known key bindings", key_bindings_configuration.name_to_key] |> invalidOp
            )
        ]
    ]

let get_key_binding_elements
    (key_bindings_configuration : Key_Bindings_Configuration)
    : ReactElement seq =

    seq {
(* Do not include permanent key bindings. *)
        yield! default_key_bindings |> Seq.collect (get_key_binding_element key_bindings_configuration)
        yield! default_debug_key_bindings |> Seq.collect (get_key_binding_element key_bindings_configuration)
    }

(* Functions - configuration *)

let private get_key_binding (binding : Key_Binding) : string =
    let element = document.getElementById $"txt_{binding.name}"
    if not <| isNull element then (element :?> HTMLTextAreaElement).value
    else error "get_key_binding" "Expected element not found." ["expected element name", $"txt_{binding.name}"] |> invalidOp

let private get_user_configured_key_bindings (bindings : Key_Binding list) : User_Configured_Key_Binding list =
    bindings |> List.map (fun binding ->
        { key = get_key_binding binding; name = binding.name; display_name = binding.display_name }
    )

let private check_key_bindings (bindings : User_Configured_Key_Binding list) : bool =
    let show_duplicate_key_bindings (duplicates) =
        duplicates
            |> List.map (fun (_, binding) -> {| name = binding.display_name; key = binding.key |})
            |> List.sortBy (fun binding -> binding.key)

    let non_valid_keys = bindings |> List.choose (fun binding ->
        if binding.key.Length <> 1 then Some binding else None 
    )
    if non_valid_keys.Length > 0 then
        warn "check_key_bindings" true "Tried to bind non-valid key." ["non-valid keys", non_valid_keys |> List.map (fun binding -> {| name = binding.display_name; key = binding.key |}) :> obj]
        false
    else
        let duplicates_1 = bindings |> duplicates_by (fun binding -> binding.key)
        if duplicates_1.Length > 0 then
            warn "check_key_bindings" true "Duplicate key bindings." ["duplicate key bindings", duplicates_1 |> show_duplicate_key_bindings :> obj]
            false
        else
(* Make sure the user did not create any key bindings that collide with permanent key bindings. This should not be an issue now, but it might later. *)
            let duplicates_2 =
                (bindings @ (permanent_key_bindings
                    |> List.map (fun binding ->
                        { key = binding.default_key; name = binding.name; display_name = binding.display_name }
                    )))
                    |> duplicates_by (fun binding -> binding.key)
            if duplicates_2.Length > 0 then
                warn "check_key_bindings" true "Key bindings collide with permanent key bindings." ["colliding key bindings", duplicates_2 |> show_duplicate_key_bindings :> obj]
                false
            else true

let get_key_bindings_configuration () : Key_Bindings_Configuration option =
    let key_to_name =
        (default_key_bindings |> get_user_configured_key_bindings) @
        (default_debug_key_bindings |> get_user_configured_key_bindings)
    if not <| check_key_bindings key_to_name then None
    else
        {
            key_to_name = key_to_name |> Seq.map (fun binding -> binding.key, binding.name) |> Map.ofSeq
            name_to_key = key_to_name |> Seq.map (fun binding -> binding.name, binding.key) |> Map.ofSeq
        } |> Some

let get_default_key_bindings_configuration () : Key_Bindings_Configuration =
    let key_to_name =
        (default_key_bindings |> List.map (fun binding -> binding.default_key, binding.name)) @
        (default_debug_key_bindings |> List.map (fun binding -> binding.default_key, binding.name))
        |> Map.ofList
    {
        key_to_name = key_to_name
        name_to_key = key_to_name |> Seq.map (fun kv -> kv.Value, kv.Key) |> Map.ofSeq
    }
