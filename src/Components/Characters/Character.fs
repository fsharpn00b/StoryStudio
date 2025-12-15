module Character

// Environment.NewLine
open System

// console
open Browser.Dom
open Elmish
open Feliz
open Feliz.UseElmish

open Character_Rendering
open Character_Types
open Fade_Transition
open Fade_Types
open Fade_Visibility
open Log
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Character"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable debug_render_counter = 1

(* Helper functions *)

let private force_complete_transition
    (dispatch : Fade_Message<Visible_Character_Data> -> unit)
    (fade_state : IRefValue<Fade_State<Visible_Character_Data>>)
    : unit =

(* Only cancel transitions for characters for whom we are running transitions. *)
    if Fade_Transition.is_running_transition fade_state then
        do force_complete_fade_transition fade_state dispatch

(* Main functions - visibility *)

let private fade_in
    (dispatch : Fade_Message<Visible_Character_Data> -> unit)
    (character_full_name : string)
    (url : string)
    (position : int<percent>)
    (height : IRefValue<int<percent>>)
    (transition_time : Fade_Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "fade_in" String.Empty ["character_full_name", character_full_name; "url", url; "position", position; "transition_time", transition_time]
    #endif

    dispatch (Fade_In {
        new_data = {
            position = position
            height = height.current
            url = url
        }
        transition_time = transition_time
        command_queue_item_id = command_queue_item_id
    })

let private fade_out
    (dispatch : Fade_Message<Visible_Character_Data> -> unit)
    (character_full_name : string)
    (transition_time : Fade_Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "fade_out" String.Empty ["character_full_name", character_full_name; "transition_time",transition_time]
    #endif

    dispatch <| Fade_Out {
        transition_time = transition_time
        command_queue_item_id = command_queue_item_id
    }

let private cross_fade
    (dispatch : Fade_Message<Visible_Character_Data> -> unit)
    (fade_state : IRefValue<Fade_State<Visible_Character_Data>>)
    (character_full_name : string)
    (url : string)
    (height : IRefValue<int<percent>>)
    (transition_time : Fade_Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "cross_fade" String.Empty ["character_full_name", character_full_name; "url", url; "transition_time",transition_time]
    #endif

(* Character position is stored in the fade state (if the state is visible) so we need to extract it. *)
    match fade_state.current with
    | Idle_Visible character_data ->
        dispatch (Cross_Fade {
            new_data = {
                position = character_data.position
                height = height.current
                url = url
            }
            transition_time = transition_time
            command_queue_item_id = command_queue_item_id
        })
    | _ -> do warn "cross_fade" false "Character not visible." ["character_full_name", character_full_name]

(* Main functions - state *)

let private get_state
    (fade_state : IRefValue<Fade_State<Visible_Character_Data>>)
    : Visible_Character_Data option =
    match fade_state.current with
    | Idle_Hidden -> None
    | Idle_Visible data -> Some data
    | Cross_Fade_Pre_Transition state_2 -> Some state_2.new_data
    | Cross_Fade_Transition state_2 -> Some state_2.new_data
    | Fade_In_Pre_Transition state_2 -> Some state_2.new_data
    | Fade_In_Transition state_2 -> Some state_2.new_data
    | Fade_Out_Pre_Transition _ -> None
    | Fade_Out_Transition _ -> None

let private set_state
    (state_1 : Visible_Character_Data option)
    (dispatch : Fade_Message<Visible_Character_Data> -> unit)
    : unit =

    match state_1 with
    | Some state_2 ->
        dispatch <| Show {
            data = state_2
            is_notify_transition_complete = false
            command_queue_item_id = None
        }
    | None ->
        dispatch <| Hide {
            is_notify_transition_complete = false
            command_queue_item_id = None
        }

(* Component *)

[<ReactComponent>]
let Character
    (props : {| expose : IRefValue<I_Character> |},
    character : Character_Input,
    character_id : int<character_id>,
    notify_transition_complete : int<command_queue_item_id> -> unit)
    : ReactElement =

(* State *)

    let id = character_id
(* Character position is only needed for fade in and cross fade. After fade in, it is stored in Visible_Character_Data, which is stored in the Idle_Visible Fade_State.
Height is set in the character definition file and not changed afterward. It is copied to Visible_Characer_Data because we use that to render the character.
*)
    let height : IRefValue<int<percent>> = React.useRef character.height
    let fade_configuration : Fade_Configuration = ()
    let fade_transition_timeout_function_handle = React.useRef None
    let fade_state, dispatch = React.useElmish((Idle_Hidden, Cmd.none), update fade_configuration fade_transition_timeout_function_handle notify_transition_complete, [||])
    let fade_state_ref = React.useRef fade_state
    do fade_state_ref.current <- fade_state

    do React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Character with
                member _.fade_in
                    (url : string)
                    (position : int<percent>)
                    (transition_time : Fade_Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    fade_in dispatch character.full_name url position height transition_time command_queue_item_id
                member _.fade_out
                    (transition_time : Fade_Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    fade_out dispatch character.full_name transition_time command_queue_item_id
                member _.cross_fade
                    (url : string)
                    (transition_time : Fade_Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    cross_fade dispatch fade_state_ref character.full_name url height transition_time command_queue_item_id
                member _.get_state () : Character_Saveable_State = get_state fade_state_ref
                member _.set_state (state : Character_Saveable_State) : unit = set_state state dispatch
                member _.get_full_name () : string = character.full_name
                member _.get_id () : int<character_id> = character_id
(* This is for debugging. *)
                member _.get_character_data () = {|
                    id = character_id
                    short_name = character.short_name
                    full_name = character.full_name
                    fade_state = fade_state_ref.current
                    fade_transition_timeout_function_handle = fade_transition_timeout_function_handle.current
                |}

            interface I_Transitionable with
                member _.is_running_transition () : bool = Fade_Transition.is_running_transition fade_state_ref
                member _.force_complete_transition () : unit = force_complete_transition dispatch fade_state_ref
                member _.get_name () : string = character.full_name
        }
    )

    view fade_state_ref
