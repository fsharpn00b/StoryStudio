module Image_Map

// String.Empty
open System

// window
open Browser.Dom
// HTMLElement
open Browser.Types
open Elmish
// ? operator
open Fable.Core.JsInterop
open Feliz
open Feliz.UseElmish

open Fade_Transition
open Fade_Types
open Fade_Visibility
open Log
open Units_Of_Measure
open Utilities

(* Types *)

type Image_Map_Item_Data = {
    value : int
    x1 : int
    y1 : int
    x2 : int
    y2 : int
    javascript_interpolations : string list
    conditional : string option
}

type Image_Map_Data = {
    name : string
    url : string
    items : Image_Map_Item_Data list
    transition_time : Fade_Transition_Time
}

type Image_Map_Item_Selected_Data = {
    name : string
    value : int
}

type Image_Map_Saveable_State =
    | Visible of Image_Map_Data
    | Hidden

(* Interfaces *)

type I_Image_Map =
(* new_data, transition_time *)
    abstract member fade_in : Image_Map_Data -> Fade_Transition_Time -> int<runner_queue_item_id> -> unit
(* transition_time *)
    abstract member fade_out : Fade_Transition_Time -> int<runner_queue_item_id> -> unit
(*
    abstract member show : Image_Map_Data -> bool -> int<runner_queue_item_id> option -> unit
    abstract member hide : bool -> int<runner_queue_item_id> option -> unit
*)
    abstract member is_visible : unit -> bool
    abstract member get_state : unit -> Image_Map_Saveable_State
    abstract member set_state : Image_Map_Saveable_State -> unit

(* Debug *)

let debug_module_name = "Image_Map"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions - rendering *)

(* TODO2 We can't cross fade between image map and background image because they use different z-indices. Plus we'd have to know whether we were cross fading between background image and image map, or between two image maps. The type info for each would be different (backround image is just a url, image map is Image_Map_Data).
We could use fade_out_all for the background, characters, and dialogue box, the continue immediately (rather than wait for callback) to the fade in for the image map. But then we would need to save the states for the background, characters, and dialogue box, and add a fade_in_all command.
*)

let private view_idle_visible
    (element_ref : IRefValue<HTMLElement option>)
    (data : Image_Map_Data)
    (notify_image_map_selection : string -> int -> unit)
    : ReactElement =

    Html.div [
(* Make sure this screen can receive focus. This is not strictly needed if we are not stopping propagation of key down events, but we are leaving it here for now in case it is useful later. *)
        prop.ref element_ref
        prop.tabIndex 0
(* Unlike in the configuration screen, we do not stop the propagation of key down events. *)
(* Prevent a mouse click from calling Runner.run (). *)
        prop.onClick (fun event -> do event.stopPropagation ())

        prop.id "image_map_container"
        prop.style [style.zIndex image_map_z_index]
        prop.children [
            Html.img [
                prop.id "image_map_image"
                prop.key data.url
                prop.src data.url
            ]

            for item in data.items do
                let left   = item.x1
                let top    = item.y1
                let width  = item.x2 - item.x1
                let height = item.y2 - item.y1

                Html.div [
                    prop.className "image_map_hotspot"
                    prop.style [
                        style.zIndex image_map_hotspot_z_index
                        style.left (length.percent left)
                        style.top (length.percent top)
                        style.width (length.percent width)
                        style.height (length.percent height)
                    ]
                    prop.onClick (fun event ->
                        do
                            event.stopPropagation ()
                            notify_image_map_selection data.name item.value
                    )
                ]
        ]
    ]

let private view_fade_in_out
    (element_ref : IRefValue<HTMLElement option>)
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

    Html.div [
(* Make sure this screen can receive focus. This is not strictly needed if we are not stopping propagation of key down events, but we are leaving it here for now in case it is useful later. *)
        prop.ref element_ref
        prop.tabIndex 0
(* Unlike in the configuration screen, we do not stop the propagation of key down events. *)
(* Prevent a mouse click from calling Runner.run (). *)
        prop.onClick (fun event -> do event.stopPropagation ())

        prop.id "image_map_container"
        prop.style [style.zIndex image_map_z_index]
        prop.children [
            Html.img [
                prop.id "image_map_image"
                prop.key url
                prop.src url
                prop.style [
                    style.opacity opacity
                    style.custom ("transition", $"opacity {transition_time}s ease-in-out")
                ]
            ]
        ]
    ]

let private view
    (element_ref : IRefValue<HTMLElement option>)
    (state : IRefValue<Fade_State<Image_Map_Data>>)
    (notify_image_map_selection : string -> int -> unit)
    : ReactElement =

    match state.current with

    | Idle_Hidden -> Html.none

    | Idle_Visible data -> view_idle_visible element_ref data notify_image_map_selection

    | Fade_In_Pre_Transition data -> view_fade_in_out element_ref true true data.new_data.url data.transition_time

    | Fade_In_Transition data -> view_fade_in_out element_ref false true data.new_data.url data.transition_time

    | Fade_Out_Pre_Transition data -> view_fade_in_out element_ref true false data.old_data.url data.transition_time

    | Fade_Out_Transition data -> view_fade_in_out element_ref false false data.old_data.url data.transition_time

    | _ -> error "view" "Unexpected state." ["state", state.current] |> invalidOp

(* For all of the following, see comments for Menu. *)

(* Main functions - state *)

let private get_state
    (state : IRefValue<Fade_State<Image_Map_Data>>)
    : Image_Map_Saveable_State =

    match state.current with
    | Idle_Hidden -> Hidden
    | Idle_Visible data -> Visible data
    | Fade_In_Pre_Transition data -> Visible data.new_data
    | Fade_In_Transition data -> Visible data.new_data
    | Fade_Out_Pre_Transition _
    | Fade_Out_Transition _ -> Hidden
    | _ -> error "get_state" "Unexpected state." ["state", state.current] |> invalidOp

let private set_state
    (fade_dispatch : Fade_Message<Image_Map_Data> -> unit)
    (saved_state : Image_Map_Saveable_State)
    : unit =

    match saved_state with
(* We do not notify Runner when the transition completes when the player loads a saved game or rolls back/forward, because we are not running a command. *)
    | Visible data ->
        fade_dispatch <| Show {
            data = data
            is_notify_transition_complete = false
            command_queue_item_id = None
        }
    | Hidden ->
        fade_dispatch <| Hide {
            is_notify_transition_complete = false
            command_queue_item_id = None
        }

(* Component *)

[<ReactComponent>]
let Image_Map
    (props : {| expose : IRefValue<I_Image_Map> |},
    notify_transition_complete : int<runner_queue_item_id> -> unit,
    notify_image_map_selection : string -> int -> unit)
    : ReactElement =

(* State *)

    let fade_transition_timeout_function_handle = React.useRef None
    let fade_configuration : Fade_Configuration = ()
    let state, dispatch = React.useElmish((Idle_Hidden, Cmd.none), update fade_configuration fade_transition_timeout_function_handle notify_transition_complete, [||])

    let state_ref = React.useRef state
    do state_ref.current <- state

(* Give focus to this component when it is visible. This is so we can prevent mouse click and key down events leaking to the game. *)
    let element_ref = React.useRef None
    React.useEffect(
        (fun () ->
            match state with
            | Idle_Hidden -> ()
            | _ -> element_ref.current?focus()
        ), [| box state |])

(* Interface *)

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Image_Map with
                member _.fade_in
                    (new_data : Image_Map_Data)
                    (transition_time : Fade_Transition_Time)
                    (command_queue_item_id : int<runner_queue_item_id>) =
                    dispatch (Fade_In {
                        new_data = new_data
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
(* Runner_UI.run () calls this method to determine whether to proceed. We want to handle the possible states as follows.
Idle_Hidden - Proceed.
Idle_Visible - Do not proceed. The player must select an image map item to continue.
Fade_In_Pre_Transition or Fade_In_Transition - Proceed. This will complete the fade in transition and change the state to Idle_Visible.
Fade_Out_Pre_Transition or Fade_Out_Transition Hidden - Proceed. This will complete the fade out transition and change the state to Idle_Hidden.
(end)
For now, Image_Map does not support states Cross_Fade_Pre_Transition or Cross_Fade_Transition.
The other three components with is_visible () methods (Menu, Save_Load, Configuration) do not have transitions, so they are not useful references.
*)
                member _.is_visible (): bool =
                    match state_ref.current with
                    | Idle_Visible _ -> true
                    | _ -> false
                member _.get_state () = get_state state_ref
                member _.set_state (saved_state : Image_Map_Saveable_State) = set_state  dispatch saved_state
            interface I_Transitionable with
                member _.is_running_transition () : bool = is_running_transition state_ref
                member _.force_complete_transition () : unit = force_complete_fade_transition state_ref dispatch
                member _.get_name () : string = "Image_Map"
        }
    )

(* Render *)

    view element_ref state_ref notify_image_map_selection
