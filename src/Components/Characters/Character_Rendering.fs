module Character_Rendering

// String.Compare, String.Empty
open System

// console, document, window
open Browser.Dom
// Html, IRefValue, IStyleAttribue, React, ReactComponent, ReactElement
open Feliz

open Character_Types
open Log
open Transition
open Units_Of_Measure

(* Debug *)

let private log_module_name = "Character_Rendering"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable debug_render_counter = 1

(* Helper functions - rendering *)

let private get_character_fade_style (position : int<percent>) (height : int<percent>) : (string * string) list =
    [
        "left", $"{position}%%"
        "height", $"{height}vh"
    ]

let private get_character_move_style (height : int<percent>) : (string * string) list = 
    [
        "height", $"{height}vh"
    ]

(* 20260418 Code added by AI. *)
let private try_get_character_width_pixels (url : string) : float option =
    let selector = $"img.character[src=\"{url}\"]"
    match document.querySelector selector with
    | null -> None
    | image ->
        let width = image.getBoundingClientRect().width
        if width > 0.0 then Some width else None

let private get_characters_container_width_pixels () : float =
    match document.getElementById "characters_container" with
    | null -> window.innerWidth
    | element ->
        let width = element.getBoundingClientRect().width
        if width > 0.0 then width else window.innerWidth
(* 20260418 End code added by AI. *)

(* Main functions - rendering *)

let private view_idle_visible
    (character_sprite : Visible_Character_Data)
    : ReactElement =

    Html.img [
        prop.className "character"
(* See
https://react.dev/learn/rendering-lists
Keys tell React which array item each component corresponds to, so that it can match them up later. This becomes important if your array items can move (e.g. due to sorting), get inserted, or get deleted. A well-chosen key helps React infer what exactly has happened, and make the correct updates to the DOM tree.
(end)
*)
        prop.key character_sprite.url
        prop.src character_sprite.url
        prop.style (get_character_fade_style character_sprite.position character_sprite.height |> List.map style.custom)
    ]

// TODO2 #transitions Add move in/out at bottom.
let view_2_move
    (transition_data : Transition_Data<Character_State, Character_Transition_Type>)
    (move_data : Character_Move_Data)
    (complete_transition : Complete_Transition_Func<Character_State>)
    : ReactElement seq =

(* The URL and height do not change during a move transition. *)
    let url, height = 
        match transition_data.old_data, transition_data.new_data with
        | Visible data, _
        | _, Visible data -> data.url, data.height
(* If both old and new data are Hidden, Transition.begin_transition () does not trigger a state change. *)
        | _ -> error "view_2_move" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp

(* 20260418 Code added by AI. *)
    let container_width = get_characters_container_width_pixels ()
(* If we cannot get the character width, use a default of 20% of the container width. *)
    let character_width = try_get_character_width_pixels url |> Option.defaultValue (container_width * 0.2)
    let offscreen_left = $"{-character_width}px"
    let offscreen_right = $"{container_width}px"
(* 20260418 End code added by AI. *)

(* As with the fade transition, we use old and new data to infer the transition (for example, Hidden -> Visible means Move_In). If the author applies Move_In to an already visible character, we just move the character to the new position. If they apply Move_Out to an already hidden character, both the old and new data are Hidden, and Transition.begin_transition () does not trigger a state change. *)
    let transition_property_initial_value, transition_property_final_value =
        match transition_data.old_data, transition_data.new_data with
(* If old and new data are the same, Transition.begin_transition () does not trigger a state change. *)
        | Visible old_data, Visible new_data ->
            $"{float old_data.position}%%", $"{float new_data.position}%%"
        | Hidden, Visible data ->
            let final_value = $"{float data.position}%%"
            match move_data.direction with
            | Left -> offscreen_left, final_value
            | Right -> offscreen_right, final_value
        | Visible data, Hidden ->
            let initial_value = $"{float data.position}%%"
            match move_data.direction with
            | Left -> initial_value, offscreen_left
            | Right -> initial_value, offscreen_right
        | _ -> error "view_2_move" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp

    let complete_transition_2 = complete_transition (Some transition_data.command_queue_item_id) true

    get_transitionable_image
        (Some "character")
        None
        []
        (get_character_move_style height)
        "left"
        transition_data.transition_time
        (fun () -> complete_transition_2 transition_data.new_data) url transition_property_initial_value transition_property_final_value
        |> Seq.singleton

// TODO2 #transitions If the author applies Fade_In to an already visible character but with a different position, we cross-fade the character at the old position, then the character jumps to the new position when we reach state Idle (Visible). This is okay for now, since the author should use Cross_Fade for an already visible character, and Move_In to change a character's position.
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
            (Some "character")
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
                | Fade _ -> view_2_fade transition_data complete_transition
                | Move move_data -> view_2_move transition_data move_data complete_transition
    ]
