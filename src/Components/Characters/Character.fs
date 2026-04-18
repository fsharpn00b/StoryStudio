module Character

// console, window
open Browser.Dom
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Character_Rendering
open Character_Types
open Log
open Transition
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Character"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable debug_render_counter = 1

(* Main functions - state *)

let private get_state
    (state : IRefValue<Transition_State<Character_State, Character_Transition_Type>>)
    : Character_State =

    match state.current with
    | Idle Hidden -> Hidden
    | Idle (Visible data) -> Visible data
    | In_Transition state_2 -> state_2.new_data

let private restore_saved_state
    (saved_state : Character_State)
    (complete_transition : Complete_Transition_Func<Character_State>)
    : unit =

    do complete_transition None false saved_state

(* Main functions - rendering *)

let private transition
    (set_state : Set_State_Func<Character_State, Character_Transition_Type>)
    (notify_transition_complete : int<command_queue_item_id> -> unit)
    (state_ref : IRefValue<Transition_State<Character_State, Character_Transition_Type>>)
(* This is for debugging.*)
    (character : Character_Input)
    (height : IRefValue<int<percent>>)
    (transition_data : Character_Transition_Data)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    let new_data, transition_time =
        match transition_data.transition with
        | Fade (Fade_In fade_data) ->
            Visible {
                position = fade_data.position
                height = height.current
                url = fade_data.url
            }, fade_data.transition_time
        | Fade (Fade_Out fade_data) ->
            Hidden, fade_data.transition_time
        | Fade (Cross_Fade fade_data) ->
(* Character position is stored in the fade state (if the state is visible) so we need to extract it. *)
            let position =
                match state_ref.current with
                | Idle (Visible data) -> data.position
                | _ -> error "cross_fade" "Tried to cross-fade a character that is not visible." ["character", character] |> invalidOp
            Visible {
                position = position
                height = height.current
                url = fade_data.url
            }, fade_data.transition_time
        | Move move_data ->
            match move_data.in_or_out with
            | Move_In move_data ->
                Visible {
                    position = move_data.position
                    height = height.current
                    url = move_data.url
                }, move_data.transition_time
            | Move_Out move_data ->
                Hidden, move_data.transition_time

    do begin_transition
        set_state
        notify_transition_complete
        state_ref
        new_data
        transition_time
        transition_data.transition
        command_queue_item_id

(* Component *)

[<ReactComponent>]
let Character
    (props : {| expose : IRefValue<I_Character> |},
    character : Character_Input,
    character_id : int<character_id>,
    notify_transition_complete : int<command_queue_item_id> -> unit)
    : ReactElement =

(* State *)

(* Character position is only needed for fade in and cross fade. After fade in, it is stored in Visible_Character_Data, which is stored in the Idle_Visible Fade_State.
Height is set in the character definition file and not changed afterward. It is copied to Visible_Characer_Data because we use that to render the character.
*)
    let height : IRefValue<int<percent>> = React.useRef character.height
    let transition_timeout_function_handle = React.useRef None
    let state, set_state = React.useState (Idle Hidden)
    let state_ref = React.useRef state
    do state_ref.current <- state
    let complete_transition_2 = complete_transition set_state notify_transition_complete

(* Interface *)

    do React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Character with
                member _.transition
                    (transition_data : Character_Transition_Data)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    
                    transition set_state notify_transition_complete state_ref character height transition_data command_queue_item_id

                member _.get_state () : Character_State = get_state state_ref
                member _.set_state (state : Character_State) : unit = restore_saved_state state complete_transition_2
                member _.get_full_name () : string = character.full_name
                member _.get_id () : int<character_id> = character_id
(* This is for debugging. *)
                member _.get_character_data () = {|
                    id = character_id
                    short_name = character.short_name
                    full_name = character.full_name
                    state = state_ref.current
                    transition_timeout_function_handle = transition_timeout_function_handle.current
                |}

            interface I_Transitionable with
                member _.is_running_transition () : bool = is_running_transition state_ref
                member _.force_complete_transition () : unit = force_complete_transition state_ref complete_transition_2
                member _.get_name () : string = character.full_name
        }
    )

    view state_ref complete_transition_2
