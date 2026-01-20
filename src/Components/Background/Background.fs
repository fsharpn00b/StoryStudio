module Background

// String.Empty
open System

// console, window
open Browser.Dom
// Cmd
open Elmish
(* 20251001 We are supposed to be using Feliz, not Fable.React. It seems IRefValue is in Fable.React, but can also be provided by Feliz. Using IRefValue only causes an error if we comment out both open Fable.React and open Feliz.
*)
(*
open Fable.React
*)
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz
// useElmish
open Feliz.UseElmish

open Fade_Transition
open Fade_Types
open Fade_Visibility
open Log
open Units_Of_Measure
open Utilities

(* Types - public *)

type Background_Configuration = {
    placeholder : unit
}

type Background_Data = string

type Background_Saveable_State =
    | Visible of string
    | Hidden

(* These types are used by the parser to create commands. *)

type Background_Fade_In_Data = {
    new_url : string
    transition_time : Fade_Transition_Time
}

type Background_Fade_Out_Data = {
    transition_time : Fade_Transition_Time
}

type Background_Cross_Fade_Data = {
    new_url : string
    transition_time : Fade_Transition_Time
}

(* Interfaces *)

type I_Background =
(* new_data, transition_time *)
    abstract member fade_in : string -> Fade_Transition_Time -> int<runner_queue_item_id> -> unit
(* transition_time *)
    abstract member fade_out : Fade_Transition_Time -> int<runner_queue_item_id> -> unit
(* new_data, transition_time *)
    abstract member cross_fade : string -> Fade_Transition_Time -> int<runner_queue_item_id> -> unit
    abstract member get_state : unit -> Background_Saveable_State
    abstract member set_state : Background_Saveable_State -> unit
    abstract member set_configuration : Background_Configuration -> unit
    abstract member get_configuration : unit -> Background_Configuration
(* This is for debugging. *)
    abstract member get_background : unit -> unit

(* Consts *)



(* Debug *)

let private log_module_name = "Background"

let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1

(* Main functions - rendering *)

let private view_idle_visible
    (url : string)
    : ReactElement =
    Html.img [
        prop.className "background_fade_image"
(* See
https://react.dev/learn/rendering-lists
Keys tell React which array item each component corresponds to, so that it can match them up later. This becomes important if your array items can move (e.g. due to sorting), get inserted, or get deleted. A well-chosen key helps React infer what exactly has happened, and make the correct updates to the DOM tree.
(end)
*)
        prop.key url
        prop.src url
    ]

(* TODO1 #future Make Fade_* more abstract so it can transition any property, not just opacity. That assumes we can use the same process, namely (1) draw very briefly with original property value, draw again with target property value, set back to idle.
For instance, if you give original and target x or y coordinates, does HTML know to animate between the two?
*)
let private view_fade_in_out
    (is_pre_transition : bool)
    (is_fade_in : bool)
    (url : string)
    (transition_time : Fade_Transition_Time)
    : ReactElement =

(* TODO2 It would probably be straightforward to generalize Fade to handle any kind of transition that can be expressed this way.
1 Render with original value and specify transition property and time.
2 Render again with final value to trigger transition.
*)
    let opacity =
        match is_fade_in, is_pre_transition with
        | true, true -> 0.0
        | true, false -> 1.0
        | false, true -> 1.0
        | false, false -> 0.0

    Html.img [
        prop.className "background_fade_image"
        prop.key url
        prop.src url
        prop.style [
            style.opacity opacity
            style.custom ("transition", $"opacity {transition_time}s ease-in-out")
        ]
    ]

(* To transition from image A to image B, we need to briefly render image A with opacity 1 and image B with opacity 0, specify fade transitions for each of them, then render image A with opacity 0 and image B with opacity 1 (the final opacity values).
*)
let private view_cross_fade
    (is_pre_transition : bool)
    (old_url : string)
    (new_url : string)
    (transition_time : Fade_Transition_Time)
    : ReactElement seq =

    [
        Html.img [
            prop.className "background_fade_image"
            prop.key old_url
            prop.src old_url
            prop.style [
                style.opacity <| if is_pre_transition then 1.0 else 0.0
(* TODO2 We can ignore string interpolation errors. They seem to result from a bug in Ionide. *)
                style.custom ("transition", $"opacity {transition_time}s ease-in-out")
            ]
        ]
        Html.img [
            prop.className "background_fade_image"
            prop.key new_url
            prop.src new_url
            prop.style [
                style.opacity <| if is_pre_transition then 0.0 else 1.0
                style.custom ("transition", $"opacity {transition_time}s ease-in-out")
            ]
        ]
    ] |> List.toSeq

let private view
    (state : IRefValue<Fade_State<Background_Data>>)
    (configuration : Background_Configuration)
    : ReactElement =

    #if debug
    do
        debug "view" String.Empty ["debug_render_counter", debug_render_counter; "state", state]
        debug_render_counter <- debug_render_counter + 1
    #endif

    match state.current with
    | Idle_Hidden -> Html.none
    | _ ->
        Html.div [
            prop.id "background_fade_container"
            prop.children [
                match state.current with
(* TODO2 This is still entangled with Fade_State, but that probably cannot be helped.
opacity could be moved to a CSS class. But src, key, and transition are not static and must be determined by code.
*)
                | Idle_Visible url -> view_idle_visible url
                | Fade_In_Pre_Transition transition_data -> view_fade_in_out true true transition_data.new_data transition_data.transition_time
                | Fade_In_Transition transition_data -> view_fade_in_out false true transition_data.new_data transition_data.transition_time
                | Fade_Out_Pre_Transition transition_data -> view_fade_in_out true false transition_data.old_data transition_data.transition_time
                | Fade_Out_Transition transition_data -> view_fade_in_out false false transition_data.old_data transition_data.transition_time
                | Cross_Fade_Pre_Transition transition_data ->
                    yield! view_cross_fade true transition_data.old_data transition_data.new_data transition_data.transition_time
                | Cross_Fade_Transition transition_data ->
                    yield! view_cross_fade false transition_data.old_data transition_data.new_data transition_data.transition_time
                | _ -> Html.none
            ]
        ]

(* Main functions - state *)

(* We would like to move this to Fade, but each UI component's get_state function must return a saveable state whose type is specific to that component. *)
// TODO2 state had type IRefValue<Fade_State<string>>. That should never have compiled, let alone worked. Same issue for set_state () below. Does IRefValue just erase the types it contains?
let private get_state (state : IRefValue<Fade_State<Background_Data>>) : Background_Saveable_State =
    match state.current with
    | Idle_Hidden -> Hidden
    | Idle_Visible data -> Visible data
    | Cross_Fade_Pre_Transition state_2 -> Visible state_2.new_data
    | Cross_Fade_Transition state_2 -> Visible state_2.new_data
    | Fade_In_Pre_Transition state_2 -> Visible state_2.new_data
    | Fade_In_Transition state_2 -> Visible state_2.new_data
    | Fade_Out_Pre_Transition _ -> Hidden
    | Fade_Out_Transition _ -> Hidden

let private set_state
    (dispatch : Fade_Message<Background_Data> -> unit)
    (current_state : IRefValue<Fade_State<Background_Data>>)
    (saved_state : Background_Saveable_State)
    : unit =

    do
(* Runner_State.undo_redo () and .show_saved_game_screen () are now responsible for forcing transition completion. *)
//        force_complete_fade_transition current_state dispatch
        match saved_state with
        | Visible data ->
            dispatch <| Show {
                data = data
                is_notify_transition_complete = false
                command_queue_item_id = None
            }
        | Hidden ->
            dispatch <| Hide {
                is_notify_transition_complete = false
                command_queue_item_id = None
            }

let private set_configuration
    (old_configuration : IRefValue<Background_Configuration>)
    (new_configuration : Background_Configuration)
    : unit =
    do old_configuration.current <- new_configuration

(* Component *)

[<ReactComponent>]
let Background
    (props : {| expose : IRefValue<I_Background> |},
    configuration : Background_Configuration,
    notify_transition_complete : int<runner_queue_item_id> -> unit)
    : ReactElement =

(* State *)

(* Previously, when we were using React, we made fade_transition_timeout_function_handle part of Fade In/Fade_Out/Cross_Fade Transition_State_Data.
However, that caused an infinite loop of state updates and re-renders. 
That should not apply here, since updates should only be triggered by dispatched messages, but we feel it is better not to take chances.

We would like fade_transition_timeout_function_handle to be internal to Fade, but Fade is only a module, not a component, and we need to create fade_transition_timeout_function_handle with React.useRef, which we believe we can only call inside a component. Alternately, if we create fade_transition_timeout_function_handle inside the Fade module, we might end up with only one fade_transition_timeout_function_handle, shared by every component that uses the Fade module, which would be wrong.
*)
    let fade_transition_timeout_function_handle = React.useRef None
    let fade_configuration : Fade_Configuration = ()
    let configuration = React.useRef configuration
    let state, dispatch = React.useElmish((Idle_Hidden, Cmd.none), update fade_configuration fade_transition_timeout_function_handle notify_transition_complete, [||])

    let state_ref = React.useRef state
(* We can use this code to detect any change to the state. *)
    #if debug
    React.useEffect(
        (fun () ->
            if state_ref.current <> state then
                do debug "Constructor" "State changed." ["Old state", state_ref.current; "New state", state]
            state_ref.current <- state
        ),
        [| box state |]
    )
    #endif
    do state_ref.current <- state

(* Interface *)

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Background with
(* TODO2 Is there a reason these component interface methods should take individual parameters instead of just the data type they are going to dispatch?
*)
                member _.fade_in
                    (new_url : string)
                    (transition_time : Fade_Transition_Time)
                    (command_queue_item_id : int<runner_queue_item_id>) =
                    dispatch (Fade_In {
                        new_data = new_url
                        transition_time = transition_time
                        command_queue_item_id = command_queue_item_id
                    })
                member _.fade_out
                    (transition_time : Fade_Transition_Time)
                    (command_queue_item_id : int<runner_queue_item_id>) =
                    dispatch (Fade_Out {
                        transition_time = transition_time
                        command_queue_item_id = command_queue_item_id
                    })
                member _.cross_fade (new_url : string) (transition_time : Fade_Transition_Time) (command_queue_item_id : int<runner_queue_item_id>) =
                    dispatch (Cross_Fade {
                        new_data = new_url
                        transition_time = transition_time
                        command_queue_item_id = command_queue_item_id
                    })
                member _.get_state () : Background_Saveable_State = get_state state_ref
                member _.set_state
                    (saved_state : Background_Saveable_State) : unit = set_state dispatch state_ref saved_state
                member _.set_configuration (new_configuration : Background_Configuration) = set_configuration configuration new_configuration
                member _.get_configuration () : Background_Configuration = configuration.current
(* This is for debugging. *)
                member _.get_background (): unit =
                    do debug "get_background" String.Empty ["state", state_ref.current; "fade_transition_timeout_function_handle", fade_transition_timeout_function_handle.current]

            interface I_Transitionable with
                member _.is_running_transition () : bool = is_running_transition state_ref
                member _.force_complete_transition () : unit = force_complete_fade_transition state_ref dispatch
                member _.get_name () : string = "Background"
        }
    )

(* Render *)

    view state_ref configuration.current
