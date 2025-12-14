module Character_Rendering

// Environment.NewLine
open System

open Feliz

open Character_Types
open Fade_Types
open Log
open Units_Of_Measure

(* Debug *)

let private log_module_name = "Character_Rendering"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable debug_render_counter = 1

(* Helper functions - rendering *)

let private get_character_style (character : Visible_Character_Data) =
    [
        style.custom ("position", "absolute")
        style.bottom 0
        style.custom ("left", $"{character.position}%%")
        style.custom ("height", $"{character.height}vh")
        style.custom ("width", "auto")
        style.zIndex 1
    ]

(* Main functions - rendering *)

let private view_idle_visible
    (character : Visible_Character_Data)
    : ReactElement =
    Html.img [
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
//        prop.className "character"
(* See
https://react.dev/learn/rendering-lists
Keys tell React which array item each component corresponds to, so that it can match them up later. This becomes important if your array items can move (e.g. due to sorting), get inserted, or get deleted. A well-chosen key helps React infer what exactly has happened, and make the correct updates to the DOM tree.
(end)
*)
        prop.key character.url
        prop.src character.url
        prop.style <| get_character_style character
    ]

let private view_fade_in_out
    (is_pre_transition : bool)
    (is_fade_in : bool)
    (character : Visible_Character_Data)
    (transition_time : Fade_Transition_Time)
    : ReactElement =

    let opacity =
        match is_fade_in, is_pre_transition with
        | true, true -> 0.0
        | true, false -> 1.0
        | false, true -> 1.0
        | false, false -> 0.0

    Html.img [
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
//        prop.className "character"
        prop.key character.url
        prop.src character.url
        prop.style (
            get_character_style character
            @ [
                style.opacity opacity
(* TODO2 We can ignore string interpolation errors. They seem to result from a bug in Ionide. *)
                style.custom ("transition", $"opacity {transition_time}s ease-in-out")
            ]
        )
    ]

let private view_cross_fade
    (is_pre_transition : bool)
    (old_character : Visible_Character_Data)
    (new_character : Visible_Character_Data)
    (transition_time : Fade_Transition_Time)
    : ReactElement seq =
    [
        Html.img [
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
//            prop.className "character"
            prop.key old_character.url
            prop.src old_character.url
            prop.style (
                get_character_style old_character
                @ [
                    style.opacity <| if is_pre_transition then 1.0 else 0.0
                    style.custom ("transition", $"opacity {transition_time}s ease-in-out")
                ]
            )
        ]
        Html.img [
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
//            prop.className "character"
            prop.key new_character.url
            prop.src new_character.url
            prop.style (
                get_character_style new_character
                @ [
                    style.opacity <| if is_pre_transition then 0.0 else 1.0
                    style.custom ("transition", $"opacity {transition_time}s ease-in-out")
                ]
            )
        ]
    ]

let view
    (fade_state : IRefValue<Fade_State<Visible_Character_Data>>)
    : ReactElement =

    #if debug
    do
        debug "view" String.Empty ["debug_render_counter", debug_render_counter; "fade_state", fade_state]
        debug_render_counter <- debug_render_counter + 1
    #endif

    Html.div [
        match fade_state.current with
        | Idle_Hidden -> Html.none
        | Idle_Visible character -> view_idle_visible character
        | Fade_In_Pre_Transition transition_data -> view_fade_in_out true true transition_data.new_data transition_data.transition_time
        | Fade_In_Transition transition_data -> view_fade_in_out false true transition_data.new_data transition_data.transition_time
        | Fade_Out_Pre_Transition transition_data -> view_fade_in_out true false transition_data.old_data transition_data.transition_time
        | Fade_Out_Transition transition_data -> view_fade_in_out false false transition_data.old_data transition_data.transition_time
        | Cross_Fade_Pre_Transition transition_data ->
            yield! view_cross_fade true transition_data.old_data transition_data.new_data transition_data.transition_time
        | Cross_Fade_Transition transition_data ->
            yield! view_cross_fade false transition_data.old_data transition_data.new_data transition_data.transition_time
    ]
