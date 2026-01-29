module Character_Rendering

// String.Compare, String.Empty
open System

// console, window
open Browser.Dom
// Html, IRefValue, IStyleAttribue, React, ReactComponent, ReactElement
open Feliz

open Character_Types
open Log
open Transition
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Character_Rendering"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable debug_render_counter = 1

(* Helper functions - rendering *)

let private get_character_fade_style (position : int<percent>) (height : int<percent>) : (string * string) list =
    [
        "position", "absolute"
        "bottom", "0"
        "left", $"{position}%%"
        "height", $"{height}vh"
        "width", "auto"
        "zIndex", $"{character_z_index}"
    ]

let private get_character_move_style (height : int<percent>) : (string * string) list = 
    [
        "position", "absolute"
        "bottom", "0"
        "height", $"{height}vh"
        "width", "auto"
        "zIndex", $"{character_z_index}"
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
        prop.style (get_character_fade_style character_sprite.position character_sprite.height |> List.map style.custom)
    ]

// TODO1 #transitions Add move in/out at bottom.
let view_2_move
    (transition_data : Transition_Data<Character_State, Character_Transition_Type>)
    (complete_transition : Complete_Transition_Func<Character_State>)
    (move_data : Character_Move_Data_1)
    : ReactElement seq =

(* Get the character URL, position, and height.
The URL and height do not change during a move operation.
If the move direction is In, the character data is in new_data. We use the position as the transition property final value.
If the move direction is Out, the character data is in old_data. We use the position as the transition property initial value.
Unlike with Fade, we do not use the old and new data (which can be either Visible or Hidden) to determine the transition operation (for example, fade in/fade out/cross fade, or move in/move out). That is specified in Character_Move_Data_1.
*)
    let url, position, height =
        match transition_data.old_data, transition_data.new_data with
        | Hidden, Visible data
        | Visible data, Hidden -> data.url, data.position, data.height
        | _ -> error "view_2_move" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp

(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)

    let complete_transition_2 = complete_transition (Some transition_data.command_queue_item_id) true

// TODO1 #transitions This assumes the character width is no more than 20%.
    let from_left = $"-20%%"
    let from_right = "100%"
    let position = $"{float position}%%"

    let transition_property_name, transition_property_initial_value, transition_property_final_value =
        match move_data.in_or_out, move_data.direction with
        | Character_Move_In_Or_Out.In, Left -> "left", from_left, position
        | Character_Move_In_Or_Out.In, Right -> "left", from_right, position
        | Character_Move_In_Or_Out.Out, Left -> "left", position, from_left
        | Character_Move_In_Or_Out.Out, Right -> "left", position, from_right
        | _ -> error "view_2_move" "Unrecognized move command." ["transition_data", transition_data; "move_data", move_data] |> invalidOp

    get_transitionable_image
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
        None
        None
        []
        (get_character_move_style height)
        transition_property_name
        transition_data.transition_time
        (fun () -> complete_transition_2 transition_data.new_data) url transition_property_initial_value transition_property_final_value
        |> Seq.singleton

let view_2_fade
    (transition_data : Transition_Data<Character_State, Character_Transition_Type>)
    (complete_transition : Complete_Transition_Func<Character_State>)
    : ReactElement seq =

(* Get the character position and height. Neither of these changes during a fade operation, so it does not matter whether we get them from old_data or new_data. *)
    let position, height =
        match transition_data.old_data, transition_data.new_data with
        | Hidden, Visible data
        | Visible data, _ -> data.position, data.height
        | _ -> error "view_2_move" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp

    let complete_transition_2 = complete_transition (Some transition_data.command_queue_item_id) true
    let get_transitionable_image_2 =
        get_transitionable_image
(* TODO2 For some reason it is impossible to use a CSS file for this component, even though it works for every other component. In any case, we don't have any static properties or styles to apply at the moment. *)
            None
            None
            []
            (get_character_fade_style position height)
            "opacity"
            transition_data.transition_time

    match transition_data.old_data, transition_data.new_data with

    | Hidden, Visible data -> get_transitionable_image_2 (fun () -> complete_transition_2 <| Visible data) data.url "0.0" "1.0" |> Seq.singleton

    | Visible data, Hidden -> get_transitionable_image_2 (fun () -> complete_transition_2 Hidden) data.url "1.0" "0.0" |> Seq.singleton

    | Visible old_data, Visible new_data when old_data <> new_data ->
        [
(* We only want one of these transitions to report when it is complete. Otherwise, we will try to remove the command queue item id from the command queue twice, and get an error. *)
            get_transitionable_image_2 (fun () -> complete_transition None false Hidden) old_data.url "1.0" "0.0"
            get_transitionable_image_2 (fun () -> complete_transition_2 <| Visible new_data) new_data.url "0.0" "1.0"
        ]

(* Transition.begin_transition () should not trigger a state change when old_data and new_data are the same (either Hidden/Hidden or Visible old_url/Visible new_url where old_url = new_url). *)
    | _ -> error "view_2" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp
    
let view
    (state : IRefValue<Transition_State<Character_State, Character_Transition_Type>>)
    (complete_transition : Complete_Transition_Func<Character_State>)
    : ReactElement =

    #if debug
    do
        debug "view" String.Empty ["debug_render_counter", debug_render_counter; "state", state.current]
        debug_render_counter <- debug_render_counter + 1
    #endif

    Html.div [
        match state.current with
        | Idle Hidden -> Html.none
        | Idle (Visible character) -> view_idle_visible character
        | In_Transition transition_data ->
            yield!
                match transition_data.transition_type with
                | Fade -> view_2_fade transition_data complete_transition
                | Move move_data -> view_2_move transition_data complete_transition move_data
    ]
