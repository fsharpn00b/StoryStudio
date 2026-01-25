module Character

// console, window
open Browser.Dom
// Cmd
open Elmish
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz
// useElmish
open Feliz.UseElmish

open Character_Rendering
open Character_Types
open Log
open Transition
open Transition_Types
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
    | Pre_Transition state_2 -> state_2.new_data
    | In_Transition state_2 -> state_2.new_data

let private set_state
    (saved_state : Character_State)
    (dispatch : Transition_Message<Character_State, Character_Transition_Type> -> unit)
    : unit =

    match saved_state with
    | Visible data ->
        dispatch <| Skip_Transition {
            new_data = Visible data
(* Both Fade and Move involve visibility. Technically we could use either transition type. The transition time is zero, and we are simply setting the character state to Idle Visible in the specified position, so no transition is rendered. *)
            transition_type = Fade
            is_notify_transition_complete = false
            command_queue_item_id = None
        }
    | Hidden ->
        dispatch <| Skip_Transition {
            new_data = Hidden
            transition_type = Fade
            is_notify_transition_complete = false
            command_queue_item_id = None
        }

(* Component *)

[<ReactComponent>]
let Character
    (props : {| expose : IRefValue<I_Character> |},
    character : Character_Input,
    character_id : int<character_id>,
    notify_transition_complete : int<runner_queue_item_id> -> unit)
    : ReactElement =

(* State *)

(* Character position is only needed for fade in and cross fade. After fade in, it is stored in Visible_Character_Data, which is stored in the Idle_Visible Fade_State.
Height is set in the character definition file and not changed afterward. It is copied to Visible_Characer_Data because we use that to render the character.
*)
    let height : IRefValue<int<percent>> = React.useRef character.height
    let transition_configuration : Transition_Configuration = ()
    let transition_timeout_function_handle = React.useRef None
    let state, dispatch = React.useElmish((Idle Hidden, Cmd.none), update transition_configuration transition_timeout_function_handle notify_transition_complete, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

(* Interface *)

    do React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Character with
                member _.fade_in
                    (new_url : string)
                    (position : int<percent>)
                    (transition_time : Fade_Transition_Time)
                    (command_queue_item_id : int<runner_queue_item_id>)
                    : unit =
                    dispatch (Transition {
                        new_data = Visible {
                            position = position
                            height = height.current
                            url = new_url
                        }
                        transition_type = Fade
                        transition_time = transition_time
                        command_queue_item_id = command_queue_item_id
                    })
                member _.fade_out
                    (transition_time : Fade_Transition_Time)
                    (command_queue_item_id : int<runner_queue_item_id>)
                    : unit =
                    dispatch (Transition {
                        new_data = Hidden
                        transition_type = Fade
                        transition_time = transition_time
                        command_queue_item_id = command_queue_item_id
                    })
                member _.cross_fade
                    (new_url : string)
                    (transition_time : Fade_Transition_Time)
                    (command_queue_item_id : int<runner_queue_item_id>)
                    : unit =

(* Character position is stored in the fade state (if the state is visible) so we need to extract it. *)
                    let position =
                        match state_ref.current with
                        | Idle (Visible data) -> data.position
                        | _ -> error "cross_fade" "Tried to cross-fade a character that is not visible." ["character", character] |> invalidOp
                    dispatch (Transition {
                        new_data = Visible {
                            position = position
                            height = height.current
                            url = new_url
                        }
                        transition_type = Fade
                        transition_time = transition_time
                        command_queue_item_id = command_queue_item_id
                    })
(* TODO1 #transitions Verify:
- For backgrounds and characters:
- Hidden -> cross fade -> fade in
- Visible -> fade in -> cross fade
- and so on.

- Should moving in an already visible character fail? We could simply move the character to the new position.
*)
                member _.move_in
                    (new_url : string)
                    (direction : Character_Move_Direction)
                    (position : int<percent>)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<runner_queue_item_id>)
                    : unit =

                    dispatch (Transition {
                        new_data = Visible {
                            position = position
                            height = height.current
                            url = new_url
                        }
                        transition_type = Move { in_or_out = Character_Move_In_Or_Out.In; direction = direction }
                        transition_time = transition_time
                        command_queue_item_id = command_queue_item_id
                    })
                member _.move_out
                    (direction : Character_Move_Direction)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<runner_queue_item_id>)
                    : unit =

                    dispatch (Transition {
                        new_data = Hidden
                        transition_type = Move { in_or_out = Character_Move_In_Or_Out.Out; direction = direction }
                        transition_time = transition_time
                        command_queue_item_id = command_queue_item_id
                    })
                member _.get_state () : Character_State = get_state state_ref
                member _.set_state (state : Character_State) : unit = set_state state dispatch
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
                member _.force_complete_transition () : unit = force_complete_transition state_ref dispatch
                member _.get_name () : string = character.full_name
        }
    )

    view state_ref
