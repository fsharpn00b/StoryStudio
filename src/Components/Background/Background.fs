module Background

// String.Compare, String.Empty
open System

// console, window
open Browser.Dom
(* 20251001 We are supposed to be using Feliz, not Fable.React. It seems IRefValue is in Fable.React, but can also be provided by Feliz. Using IRefValue only causes an error if we comment out both open Fable.React and open Feliz.
*)
(*
open Fable.React
*)
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Log
open Transition
open Units_Of_Measure
open Utilities

(* Types - public *)

// TODO1 #background This could be used to set the background resolution, if we offer multiple resolutions.
type Background_Configuration = {
    placeholder : unit
}

type Background_State =
    | Visible of string
    | Hidden

type Background_Transition_Type = Fade

(* These types are used by the parser to create commands. *)

type Background_Fade_In_Data = {
    new_url : string
    transition_time : Transition_Time
}

type Background_Fade_Out_Data = {
    transition_time : Transition_Time
}

type Background_Cross_Fade_Data = {
    new_url : string
    transition_time : Transition_Time
}

(* Interfaces *)

type I_Background =
(* new_data, transition_time *)
    abstract member fade_in : string -> Transition_Time -> int<command_queue_item_id> -> unit
(* transition_time *)
    abstract member fade_out : Transition_Time -> int<command_queue_item_id> -> unit
(* new_data, transition_time *)
    abstract member cross_fade : string -> Transition_Time -> int<command_queue_item_id> -> unit
    abstract member get_state : unit -> Background_State
    abstract member set_state : Background_State -> unit
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
(* See
https://react.dev/learn/rendering-lists
Keys tell React which array item each component corresponds to, so that it can match them up later. This becomes important if your array items can move (e.g. due to sorting), get inserted, or get deleted. A well-chosen key helps React infer what exactly has happened, and make the correct updates to the DOM tree.
(end)
*)
        prop.key url
        prop.src url
        prop.className "background_fade_image"
    ]

(* TODO2 #future Make Fade_* more abstract so it can transition any property, not just opacity. As of 20260126 we have done so, though we still have to
1 Determine how different transition types (fade, move, and so on) compose, per component.
2 Implement view_* helpers for each transition type for each component.
*)

let private view_2
    (transition_data : Transition_Data<Background_State, Background_Transition_Type>)
    (complete_transition : Complete_Transition_Func<Background_State>)
    : ReactElement seq =

    let complete_transition_2 = complete_transition (Some transition_data.command_queue_item_id) true
    let get_transitionable_image_2 =
        get_transitionable_image
            (Some "background_fade_image")
            None
            []
            [
                "zIndex", $"{background_z_index}"
            ]
            "opacity"
            transition_data.transition_time

    match transition_data.old_data, transition_data.new_data with

    | Hidden, Visible url -> get_transitionable_image_2 (fun () -> complete_transition_2 <| Visible url) url "0.0" "1.0" |> Seq.singleton

    | Visible url, Hidden -> get_transitionable_image_2 (fun () -> complete_transition_2 Hidden) url "1.0" "0.0" |> Seq.singleton

    | Visible old_url, Visible new_url when 0 <> String.Compare (old_url, new_url) ->
        [
(* We only want one of these transitions to report when it is complete. Otherwise, we will try to remove the command queue item id from the command queue twice, and get an error. *)
// TODO2 #transitions Consider a separate component for cross fade.
            get_transitionable_image_2 (fun () -> complete_transition None false Hidden) old_url "1.0" "0.0"
            get_transitionable_image_2 (fun () -> complete_transition_2 <| Visible new_url) new_url "0.0" "1.0"
        ]

(* Transition.begin_transition () should not trigger a state change when old_data and new_data are the same (either Hidden/Hidden or Visible old_url/Visible new_url where old_url = new_url). *)
    | _ -> error "view_2" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp

let private view
    (state : IRefValue<Transition_State<Background_State, Background_Transition_Type>>)
    (configuration : Background_Configuration)
    (complete_transition : Complete_Transition_Func<Background_State>)
    : ReactElement =

    #if debug
    do
        debug "view" String.Empty ["debug_render_counter", debug_render_counter; "state", state]
        debug_render_counter <- debug_render_counter + 1
    #endif

    match state.current with
    | Idle Hidden -> Html.none
    | _ ->
        Html.div [
            prop.id "background_fade_container"
            prop.children [
                match state.current with
(* TODO2 This is still entangled with Fade_State, but that probably cannot be helped.
opacity could be moved to a CSS class. But src, key, and transition are not static and must be determined by code.
20260124 This is less true now that Fade_* has been abstracted to Transition_*.
*)
                | Idle Hidden -> Html.none
                | Idle (Visible url) -> view_idle_visible url
                | In_Transition transition_data ->
                    yield! view_2 transition_data complete_transition
            ]
        ]

(* Main functions - state *)

(* We would like to move this to Fade, but each UI component's get_state function must return a saveable state whose type is specific to that component. *)
// TODO2 state had type IRefValue<Fade_State<string>>. That should never have compiled, let alone worked. Same issue for set_state () below. Does IRefValue just erase the types it contains?
let private get_state
    (state : IRefValue<Transition_State<Background_State, Background_Transition_Type>>)
    : Background_State =

    #if debug
    do debug "get_state" String.Empty ["state", state.current]
    #endif

    match state.current with
    | Idle Hidden -> Hidden
    | Idle (Visible data) -> Visible data
    | In_Transition state_2 -> state_2.new_data

let private restore_saved_state
    (saved_state : Background_State)
    (complete_transition : Complete_Transition_Func<Background_State>)
    : unit =

    #if debug
    do debug "get_state" String.Empty ["state", saved_state]
    #endif

(* Notes
- Runner_State.undo_redo () and .show_saved_game_screen () are now responsible for forcing existing transition completion.
- Since we are not running a command, we set is_notify_transition_complete to false.
(end)
*)
    do complete_transition None false saved_state

(* Container component *)

[<ReactComponent>]
let Background
    (props : {| expose : IRefValue<I_Background> |},
    configuration : Background_Configuration,
    notify_transition_complete : int<command_queue_item_id> -> unit)
    : ReactElement =

(* State *)

    let transition_configuration : Transition_Configuration = ()
    let configuration = React.useRef configuration
    let state, set_state = React.useState (Idle Hidden)

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
(* We need this because the React.useRef that initially links state_ref to the value of state runs only once. This runs every time the component is rendered. *)
    do state_ref.current <- state

    let complete_transition_2 = complete_transition set_state notify_transition_complete

(* Interface *)

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Background with
(* TODO2 Is there a reason these component interface methods should take individual parameters instead of just the data type they are going to dispatch?
*)
                member _.fade_in
                    (new_url : string)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    begin_transition set_state notify_transition_complete state_ref (Visible new_url) transition_time Fade command_queue_item_id

                member _.fade_out
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    begin_transition set_state notify_transition_complete state_ref Hidden transition_time Fade command_queue_item_id

                member _.cross_fade
                    (new_url : string)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    begin_transition set_state notify_transition_complete state_ref (Visible new_url) transition_time Fade command_queue_item_id

                member _.get_state () : Background_State = get_state state_ref
                member _.set_state (saved_state : Background_State) : unit = restore_saved_state saved_state complete_transition_2
                member _.set_configuration (new_configuration : Background_Configuration) = do configuration.current <- new_configuration
                member _.get_configuration () : Background_Configuration = configuration.current
(* This is for debugging. *)
                member _.get_background (): unit =
                    do debug "get_background" String.Empty ["state", state_ref.current]

            interface I_Transitionable with
                member _.is_running_transition () : bool = is_running_transition state_ref
                member _.force_complete_transition () : unit = force_complete_transition state_ref complete_transition_2
                member _.get_name () : string = "Background"
        }
    )

(* Render *)

    view state_ref configuration.current complete_transition_2
