module Image_Map

// console, window
open Browser.Dom
// HTMLElement
open Browser.Types
// ? operator
open Fable.Core.JsInterop
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Log
open Transition
open Units_Of_Measure
open Utilities

(* Types *)

type Image_Map_Transition_Type = Fade

(* TODO1 #future #image_map We should let the author define a shape we can pass straight through to HTML. Depends on what shapes a div supports.
That would also let the user put labels or onMouseOver handlers in the hotspot.
*)
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
    transition_time : Transition_Time
}

type Image_Map_Item_Selected_Data = {
    name : string
    value : int
}

type Image_Map_State =
    | Visible of Image_Map_Data
    | Hidden

(* Interfaces *)

type I_Image_Map =
(* new_data, transition_time *)
    abstract member fade_in : Image_Map_Data -> Transition_Time -> int<command_queue_item_id> -> unit
(* transition_time *)
    abstract member fade_out : Transition_Time -> int<command_queue_item_id> -> unit
(*
    abstract member show : Image_Map_Data -> bool -> int<runner_queue_item_id> option -> unit
    abstract member hide : bool -> int<runner_queue_item_id> option -> unit
*)
    abstract member is_visible : unit -> bool
    abstract member get_state : unit -> Image_Map_State
    abstract member set_state : Image_Map_State -> unit

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
    (data : Image_Map_Data)
    (notify_image_map_selection : string -> int -> unit)
    : ReactElement seq =

    [
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

let private view_2
    (transition_data : Transition_Data<Image_Map_State, Image_Map_Transition_Type>)
    (complete_transition : Complete_Transition_Func<Image_Map_State>)
    : ReactElement =

    let complete_transition_2 = complete_transition (Some transition_data.command_queue_item_id) true
    let get_transitionable_image_2 =
        get_transitionable_image
            None
            (Some "image_map_image")
            []
            [
                "zIndex", $"{background_z_index}"
            ]
            "opacity"
            transition_data.transition_time

    match transition_data.old_data, transition_data.new_data with

    | Hidden, Visible data -> get_transitionable_image_2 (fun () -> complete_transition_2 <| Visible data) data.url "0.0" "1.0"
    
    | Visible data, Hidden -> get_transitionable_image_2 (fun () -> complete_transition_2 Hidden) data.url "1.0" "0.0"

    | _ -> error "view_2" "Called with unexpected transition data." ["transition_data", transition_data] |> invalidOp

let private view
    (element_ref : IRefValue<HTMLElement option>)
    (state : IRefValue<Transition_State<Image_Map_State, Image_Map_Transition_Type>>)
    (notify_image_map_selection : string -> int -> unit)
    (complete_transition : Complete_Transition_Func<Image_Map_State>)
    : ReactElement =

    match state.current with
    | Idle Hidden -> Html.none
    | _ ->
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
                match state.current with
                | Idle Hidden -> Html.none
                | Idle (Visible data) -> yield! view_idle_visible data notify_image_map_selection
                | In_Transition transition_data -> view_2 transition_data complete_transition
            ]
        ]

(* For all of the following, see comments for Menu. *)

(* Main functions - state *)

let private get_state
    (state : IRefValue<Transition_State<Image_Map_State, Image_Map_Transition_Type>>)
    : Image_Map_State =

    match state.current with
    | Idle Hidden -> Hidden
    | Idle (Visible data) -> Visible data
    | In_Transition data -> data.new_data

let private restore_saved_state
    (saved_state : Image_Map_State)
    (complete_transition : Complete_Transition_Func<Image_Map_State>)
    : unit =
(* We do not notify Runner when the transition completes when the player loads a saved game or rolls back/forward, because we are not running a command. *)
    do complete_transition None false saved_state

(* Component *)

[<ReactComponent>]
let Image_Map
    (props : {| expose : IRefValue<I_Image_Map> |},
    notify_transition_complete : int<command_queue_item_id> -> unit,
    notify_image_map_selection : string -> int -> unit)
    : ReactElement =

(* State *)

    let fade_configuration : Transition_Configuration = ()
    let state, set_state = React.useState (Idle Hidden)
    let state_ref = React.useRef state
    do state_ref.current <- state
    let complete_transition_2 = complete_transition set_state notify_transition_complete

(* Give focus to this component when it is visible. This is so we can prevent mouse click and key down events leaking to the game. *)
    let element_ref = React.useRef None
    React.useEffect(
        (fun () ->
            match state with
            | Idle Hidden -> ()
            | _ -> element_ref.current?focus()
        ), [| box state |])

(* Interface *)

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Image_Map with
// TODO2 #transitions Clicking during fade in is not completing transition. Could be because we ignore mouse clicks/run commands so the player cannot run the next command until they make a choice.
                member _.fade_in
                    (new_data : Image_Map_Data)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    begin_transition set_state notify_transition_complete state_ref (Visible new_data) transition_time Fade command_queue_item_id

                member _.fade_out
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    begin_transition set_state notify_transition_complete state_ref Hidden transition_time Fade command_queue_item_id

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
                    | Idle (Visible _) -> true
                    | _ -> false
                member _.get_state () = get_state state_ref
                member _.set_state (saved_state : Image_Map_State) = restore_saved_state saved_state complete_transition_2
            interface I_Transitionable with
                member _.is_running_transition () : bool = is_running_transition state_ref
                member _.force_complete_transition () : unit = force_complete_transition state_ref complete_transition_2
                member _.get_name () : string = "Image_Map"
        }
    )

(* Render *)

    view element_ref state_ref notify_image_map_selection complete_transition_2
