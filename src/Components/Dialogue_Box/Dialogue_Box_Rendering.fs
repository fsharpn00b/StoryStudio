module Dialogue_Box_Rendering

// console, window
open Browser.Dom
// Html, IRefValue, ReactElement
open Feliz

open Dialogue_Box_Types
open Fade_Types
open Log
open Utilities

(* Debug *)

let private log_module_name = "Dialogue_Box_Rendering"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1
let mutable private debug_state_update_counter = 1

(* Main functions - rendering *)

let view
    (typing_state : IRefValue<Typing_State>)
    (fade_state : IRefValue<Fade_State<unit>>)
    (configuration : Dialogue_Box_Configuration)
    : ReactElement=

    let get_dialogue_box (character : string) (text : string) : ReactElement =
        Html.div [
            prop.id "dialogue_box_container"
            prop.key "dialogue_box_container"
            prop.style [style.zIndex dialogue_box_z_index]
            prop.children [
                Html.div [
                    prop.id "dialogue_box_character_name_label"
                    prop.key "dialogue_box_character_name_label"
                    prop.style [style.zIndex dialogue_box_character_name_z_index]
                    prop.text character
                ]
                Html.div [
                    prop.id "dialogue_box"
                    prop.key "dialogue_box"
                    prop.children [
                        Html.div [
                            prop.id "dialogue_text"
                            prop.key "dialogue_text"
                            prop.text text
                        ]
                    ]
                ]
            ]
        ]

    #if debug
    do
        debug "view" String.Empty ["debug_render_counter", debug_render_counter; "typing_state", typing_state]
        debug_render_counter <- debug_render_counter + 1
    #endif

(* It is up to each component's view function to determine how the different states stack. In this case, we check the fade state first, to see if the dialogue box is visible, then we deal with the typing state.
See also get_state ().
*)
    match fade_state.current with
    | Idle_Hidden -> Html.none
    | Idle_Visible _ ->
        match typing_state.current with
        | Empty -> Html.none
        | Idle dialogue -> get_dialogue_box dialogue.character_full_name dialogue.text
        | Typing dialogue -> get_dialogue_box dialogue.character dialogue.visible_text
    | _ ->
        do warn "view" false "Unexpected Fade_State. Ignoring." ["fade_state", fade_state]
        Html.none
