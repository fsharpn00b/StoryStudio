module Character_Rendering

// String.Compare, String.Empty
open System

// console, window
open Browser.Dom
// Html, IRefValue, IStyleAttribue, React, ReactComponent, ReactElement
open Feliz

open Character_Types
open Log
open Transition_Types
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Character_Rendering"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable debug_render_counter = 1

(* Helper functions - rendering *)

let private get_character_fade_style (character_sprite : Visible_Character_Data) : IStyleAttribute list =
    [
        style.custom ("position", "absolute")
        style.bottom 0
        style.custom ("left", $"{character_sprite.position}%%")
        style.custom ("height", $"{character_sprite.height}vh")
        style.custom ("width", "auto")
        style.zIndex character_z_index
    ]

(* Main functions - rendering *)

let private view_idle_visible
    (character_sprite : Visible_Character_Data)
    : ReactElement =

    Html.img [
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
//        prop.className "character"
(* See
https://react.dev/learn/rendering-lists
Keys tell React which array item each component corresponds to, so that it can match them up later. This becomes important if your array items can move (e.g. due to sorting), get inserted, or get deleted. A well-chosen key helps React infer what exactly has happened, and make the correct updates to the DOM tree.
(end)
*)
        prop.key character_sprite.url
        prop.src character_sprite.url
        prop.style <| get_character_fade_style character_sprite
    ]

let private view_fade_in_out
    (is_pre_transition : bool)
    (is_fade_in : bool)
    (character_sprite : Visible_Character_Data)
    (transition_time : Transition_Time)
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
        prop.key character_sprite.url
        prop.src character_sprite.url
        prop.style (
            get_character_fade_style character_sprite
            @ [
                style.opacity opacity
                style.custom ("transition", $"opacity {transition_time}s ease-in-out")
            ]
        )
    ]

let private view_cross_fade
    (is_pre_transition : bool)
    (old_character_data : Visible_Character_Data)
    (new_character_data : Visible_Character_Data)
    (transition_time : Transition_Time)
    : ReactElement seq =
    [
        Html.img [
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
//            prop.className "character"
            prop.key old_character_data.url
            prop.src old_character_data.url
            prop.style (
                get_character_fade_style old_character_data
                @ [
                    style.opacity <| if is_pre_transition then 1.0 else 0.0
                    style.custom ("transition", $"opacity {transition_time}s ease-in-out")
                ]
            )
        ]
        Html.img [
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
//            prop.className "character"
            prop.key new_character_data.url
            prop.src new_character_data.url
            prop.style (
                get_character_fade_style new_character_data
                @ [
                    style.opacity <| if is_pre_transition then 0.0 else 1.0
                    style.custom ("transition", $"opacity {transition_time}s ease-in-out")
                ]
            )
        ]
    ]

let private view_move
    (is_pre_transition : bool)
    (move_data : Character_Move_Data_1)
    (character_data : Visible_Character_Data)
    (transition_time : Transition_Time)
    : ReactElement =

    #if debug
    debug "view_move" String.Empty ["move_data", move_data; "character_data", character_data; "is_pre_transition", is_pre_transition; "transition_time", transition_time]
    #endif

// TODO1 #transitions Add move in/out at bottom.
    let out_left_style =
        [
            style.bottom 0
            style.custom ("left", $"-{float character_data.position}%%")
            style.custom ("transition", $"left {transition_time}s ease-in-out")
        ]

    let out_right_style =
        [
            style.bottom 0
            style.custom ("left", $"{100.0 + float character_data.position}%%")
            style.custom ("transition", $"left {transition_time}s ease-in-out")
        ]

    let in_style =
        [
            style.bottom 0
            style.custom ("left", $"{float character_data.position}%%")
            style.custom ("transition", $"left {transition_time}s ease-in-out")
        ]

    let move_style =
        match move_data.in_or_out, move_data.direction, is_pre_transition with
        | Character_Move_In_Or_Out.In, Left, true -> out_left_style
        | Character_Move_In_Or_Out.In, Left, false -> in_style
        | Character_Move_In_Or_Out.Out, Left, true -> in_style
        | Character_Move_In_Or_Out.Out, Left, false -> out_left_style
        | Character_Move_In_Or_Out.In, Right, true -> out_right_style
        | Character_Move_In_Or_Out.In, Right, false -> in_style
        | Character_Move_In_Or_Out.Out, Right, true -> in_style
        | Character_Move_In_Or_Out.Out, Right, false -> out_right_style
        | _ -> error "view_move" "Unrecognized move command." ["move_data", move_data; "character_data", character_data; "is_pre_transition", is_pre_transition; "transition_time", transition_time] |> invalidOp

    Html.img [
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
//        prop.className "character"
        prop.key character_data.url
        prop.src character_data.url
        prop.style [
            style.custom ("position", "absolute")
            style.custom ("height", $"{character_data.height}vh")
            style.custom ("width", "auto")
            style.zIndex character_z_index
            yield! move_style
        ]
    ]

let view_2
    (is_pre_transition : bool)
    (transition_data : Transition_Data<Character_State, Character_Transition_Type>)
    : ReactElement seq =

    match transition_data.transition_type with
    | Fade ->
        match transition_data.old_data, transition_data.new_data with
        | Hidden, Visible data -> view_fade_in_out is_pre_transition true data transition_data.transition_time |> Seq.singleton
        | Visible url, Hidden -> view_fade_in_out is_pre_transition false url transition_data.transition_time |> Seq.singleton
        | Visible old_data, Visible new_data when old_data <> new_data ->
            view_cross_fade is_pre_transition old_data new_data transition_data.transition_time
(* Transition.update_transition () should not trigger a state change when old_data and new_data are the same (either Hidden/Hidden or Visible old_url/Visible new_url where old_url = new_url). *)
        | _ -> error "view_2" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp
    | Move move_data ->
        match transition_data.old_data, transition_data.new_data with
        | Hidden, Visible data -> view_move is_pre_transition move_data data transition_data.transition_time |> Seq.singleton
        | Visible data, Hidden -> view_move is_pre_transition move_data data transition_data.transition_time |> Seq.singleton
        | _ -> error "view_2" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp

let view
    (state : IRefValue<Transition_State<Character_State, Character_Transition_Type>>)
    : ReactElement =

    #if debug
    do
        debug "view" String.Empty ["debug_render_counter", debug_render_counter; "state", state]
        debug_render_counter <- debug_render_counter + 1
    #endif

    Html.div [
        match state.current with
        | Idle Hidden -> Html.none
        | Idle (Visible character) -> view_idle_visible character
        | Pre_Transition transition_data -> yield! view_2 true transition_data
        | In_Transition transition_data -> yield! view_2 false transition_data
    ]
