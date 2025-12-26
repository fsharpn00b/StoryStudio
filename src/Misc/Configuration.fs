module Configuration

// DateTime
open System

// navigator, Types
open Browser
// a, Element, HTMLCanvasElement
open Browser.Types
open Elmish
open Feliz
open Feliz.UseElmish
// Decode, Encode
open Thoth.Json

open Background
open Character_Types
open Dialogue_Box_Types
open Log
open Units_Of_Measure
open Utilities

(* Debug *)

let debug_module_name = "Configuration"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

type Runner_Configuration = {
(* These fields cannot be IRefValue because we need to serialize this type. *)
    background_configuration : Background_Configuration
    characters_configuration : Characters_Configuration
    dialogue_box_configuration : Dialogue_Box_Configuration
}

type Configuration_State = {
    is_visible : bool
}

type Configuration_Message =
    | Show
    | Hide

(* Interfaces *)

type I_Configuration =
    abstract member show : unit -> unit
    abstract member hide : unit -> unit
    abstract member is_visible : unit -> bool

(* Consts *)

let private local_storage_name = "vnf_configuration"

let private initial_state = {
    is_visible = false
}

(* Helper functions *)

let private characters_per_second_to_delay_between_characters
    (characters_per_second : int)
    : int<milliseconds>
    =
    int (1000 / characters_per_second) |> LanguagePrimitives.Int32WithMeasure

let private delay_between_characters_to_characters_per_second
    (delay_between_characters : int)
    : int =
    int (1000 / delay_between_characters)

let get_configuration_from_local_storage () : Runner_Configuration option =
    match localStorage.getItem local_storage_name with
    | null -> None
    | json ->
        match Decode.Auto.fromString<Runner_Configuration> json with
        | Ok configuration -> Some configuration
        | _ -> error "get_configuration_from_local_storage" "Failed to deserialize configuration." ["json", json] |> invalidOp

let private set_configuration_in_local_storage
    (configuration : IRefValue<Runner_Configuration>) : unit =
    let json = Encode.Auto.toString (0, configuration.current)
    do localStorage.setItem (local_storage_name, json)

let private update_typing_speed
    (configuration : IRefValue<Runner_Configuration>)
    (set_configuration : Runner_Configuration -> unit)
    (new_value_1 : HTMLTextAreaElement)
    : unit =

    match Int32.TryParse new_value_1.value with
    | true, new_value_2 ->
        let new_value_3 =
            if new_value_2 > 0 then characters_per_second_to_delay_between_characters new_value_2
            else 0<milliseconds>
        do
            configuration.current <- { configuration.current with dialogue_box_configuration = { typing_speed = new_value_3 }}
            set_configuration configuration.current
            set_configuration_in_local_storage configuration

    | _ -> ()

(* Main functions - rendering *)

let private view
    (configuration : IRefValue<Runner_Configuration>)
    (set_configuration : Runner_Configuration -> unit)
    (state : IRefValue<Configuration_State>)
    (dispatch : Configuration_Message -> unit)
    : ReactElement =

    if state.current.is_visible then
        Html.div [
            prop.className "configuration_screen"
            prop.style [style.zIndex configuration_z_index]
            prop.children [
                Html.div [
                    prop.className "configuration_header"
                    prop.children [
                        Html.h3 [
                            prop.text $"Configuration (Escape to exit)"
                        ]
                    ]
                ]
                Html.div [
                    prop.className "configuration_dialogue"
                    prop.children [
                        Html.label [
                            prop.text "Dialogue typing speed (characters per second, 0 = show all at once): "
                        ]
                        Html.input [
                            prop.id "txt_typing_speed"
(* It seems we are supposed to use this instead of ``type``. *)
                            prop.type' "text"
                            prop.maxLength 3
                            prop.style [
                                style.width (length.em 4)
                            ]
                            prop.defaultValue (configuration.current.dialogue_box_configuration.typing_speed |> int |> delay_between_characters_to_characters_per_second)
                        ]
                    ]
                ]
                Html.div [
                    prop.className "controls"
                    prop.children [
                        Html.button [
                            prop.text "Save"
                            prop.onClick (fun _ ->
                                do
                                    update_typing_speed configuration set_configuration (document.getElementById "txt_typing_speed"  :?> HTMLTextAreaElement)
                                    dispatch Hide
                            )
                        ]
                    ]
                ]
            ]
        ]
    else Html.none

(* Main functions - state *)

let private update
    (message : Configuration_Message)
    (state : Configuration_State)
    : Configuration_State * Cmd<Configuration_Message> =

    match message with

    | Show ->
        {
            state with
                is_visible = true
        }, Cmd.none

    | Hide -> { state with is_visible = false }, Cmd.none

(* Component *)

[<ReactComponent>]
let Configuration
    (props : {| expose : IRefValue<I_Configuration> |},
    (configuration : IRefValue<Runner_Configuration>),
(* We cannot simply change the configuration here. We must propagate the changes to the individual components, which is what set_configuration () does. The configuration cannot contain references to individual components because we must serialize and deserialize it, and we would have to serialize and deserialize the individual component configurations.
*)
    (set_configuration : Runner_Configuration -> unit)
    )
    : ReactElement =

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Configuration with
(* The configuration screen does not need to notify transition complete. It does not have a Command_Behavior, and should not add to the history or call get_next_command ().
*)
                member _.show () = dispatch <| Show
                member _.hide () = dispatch <| Hide
                member _.is_visible (): bool = state_ref.current.is_visible
        }
    )

(* Render *)

    view configuration set_configuration state_ref dispatch
