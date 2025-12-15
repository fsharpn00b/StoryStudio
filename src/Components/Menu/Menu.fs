module Menu

// String.Empty
open System

// window
open Browser.Dom
open Elmish
open Feliz
open Feliz.UseElmish

open Log
open Units_Of_Measure

(* Types - public *)

type Menu_Item_Data = {
    value : int
    text : string
    javascript_interpolations : string list
    conditional : string option
}

type Menu_Data = {
    name : string
    description : string
    javascript_interpolations : string list
    items : Menu_Item_Data list
}

type Show_Menu_Data = {
    data : Menu_Data
    is_notify_transition_complete : bool
    command_queue_item_id : int<command_queue_item_id> option
}

type Hide_Menu_Data = {
    is_notify_transition_complete : bool
    command_queue_item_id : int<command_queue_item_id> option
}

type Menu_Item_Selected_Data = {
    name : string
    value : int
}

type Menu_Message =
    | Show of Show_Menu_Data
    | Hide of Hide_Menu_Data
    | Notify_Transition_Complete of int<command_queue_item_id>
    | Menu_Item_Selected of Menu_Item_Selected_Data

type Menu_Saveable_State =
    | Visible of Menu_Data
    | Hidden

(* Interfaces *)

type I_Menu =
    abstract member show : Menu_Data -> bool -> int<command_queue_item_id> option -> unit
    abstract member hide : bool -> int<command_queue_item_id> option -> unit
    abstract member is_visible : unit -> bool
    abstract member get_state : unit -> Menu_Saveable_State
    abstract member set_state : Menu_Saveable_State -> unit

(* Debug *)

let debug_module_name = "Menu"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions - rendering *)

let private view
    (visible : IRefValue<bool>)
    (menu_data_1 : IRefValue<Menu_Data option>)
    (dispatch : Menu_Message -> unit)
    : ReactElement =

    if visible.current then
        match menu_data_1.current with
        | None -> error "view" "visible is true, but menu_data is missing." [] |> invalidOp
        | Some menu_data_2 ->
            Html.div [
                prop.id "menu_container"
                prop.children [
                    Html.div [
                        prop.id "menu_description"
                        prop.text menu_data_2.description
                    ]
                    Html.ul [
                        prop.id "menu"
                        prop.children [
                            for item in menu_data_2.items do
                                Html.li [
                                    prop.className "menu_item"
                                    prop.text item.text
                                    prop.onClick (fun _ ->
                                        dispatch <| Menu_Item_Selected {
                                            name = menu_data_2.name
                                            value = item.value
                                        })
                                ]
                        ]
                    ]
                ]
            ]
    else Html.none

(* Main functions - state *)

let private get_state
    (is_visible : IRefValue<bool>)
//    (command_queue_item_id : IRefValue<int<command_queue_item_id> option>)
    (menu_data_1 : IRefValue<Menu_Data option>)
    : Menu_Saveable_State =
    match is_visible.current, menu_data_1.current with
    | true, Some menu_data_2 ->
        Visible menu_data_2
    | _ -> Hidden

let private set_state
    (saved_state : Menu_Saveable_State)
    (dispatch : Menu_Message -> unit)
    : unit =
    match saved_state with
(* We do not notify Runner when the transition completes when the player loads a saved game or rolls back/forward. *)
    | Visible data ->
        dispatch <| Show {
            data = data
            is_notify_transition_complete = false
            command_queue_item_id = None
        }
    | Hidden -> dispatch <| Hide { is_notify_transition_complete = false; command_queue_item_id = None }

let private update
    (notify_transition_complete : int<command_queue_item_id> -> unit)
    (notify_menu_selection : string -> int -> unit)
    (menu_data : IRefValue<Menu_Data option>)
    (message : Menu_Message)
    (is_visible : bool)
    : bool * Cmd<Menu_Message> =

    match message with

    | Show data ->

        let command =
            if data.is_notify_transition_complete then
                match data.command_queue_item_id with
                | Some command_queue_item_id -> command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg
                | None -> error "update/Show" "Show_Menu_Data.is_notify_transition_complete is true, but Show_Menu_Data.command_queue_item_id is None." ["Show_Menu_Data", data] |> invalidOp
            else Cmd.none

        if not is_visible then do menu_data.current <- Some data.data
(* If the Show message was dispatched by the show () interface method, meaning it came from a command, we should notify Runner the transition is complete. If the Show message was dispatched by set_state (), meaning the player loaded a saved game or called undo or redo, we should not notify Runner, because we do not want to automatically continue to the next command. That should not happen anyway with undo or redo, because the history only includes points at which we do not automatically continue anyway (typically, this means points where we are waiting for player input). For the moment, it should also not happen with loading a saved game, because we do not automatically continue from that either, but we could change that behavior at some point.
*)
        true, command

    | Hide data ->

        let command =
            if data.is_notify_transition_complete then
                match data.command_queue_item_id with
                | Some command_queue_item_id -> command_queue_item_id |> Notify_Transition_Complete |> Cmd.ofMsg
                | None -> error "update/Hide" "Hide_Menu_Data.is_notify_transition_complete is true, but Hide_Menu_Data.command_queue_item_id is None." ["Hide_Menu_Data", data] |> invalidOp
            else Cmd.none

        if is_visible then do menu_data.current <- None
(* See the comments for Show. There is one additional case here: the hide () method was called from notify_menu_selection (). In that case, notify_menu_selection () calls get_next_command () itself, rather than use notify_transition_complete (). *)
        false, command

(* We must delay before notifying Runner the transition is complete. Otherwise, Runner receives the message before is_visible is updated. Runner_State.get_state () then records an incorrect value for is_visible, which leads to unwanted behavior for save/load and undo/redo.

We now always delay in Runner_Transition.get_notify_transition_complete (). See the notes in Runner_Transition.get_notify_transition_complete (), Fade_Visibility.update_hide (), and Fade_Transition.update_complete_transition ().

We can only get away with not delaying notification, when determining whether to run the next command, because Runner_Run.wait_for_commands_to_run () waits on a specific state value for each component (namely Idle).
*)
    | Notify_Transition_Complete (command_queue_item_id : int<command_queue_item_id>) ->
        do notify_transition_complete command_queue_item_id
        is_visible, Cmd.none

(* We do not want to call notify_transition_complete () because the command behavior for Menu is Wait_For_Callback/wait_for_callback false, and that is still the current command, so notify_transition_complete () will not call get_next_command (). Instead, we call notify_menu_selection (), which does call get_next_command (). In any case, the player selecting a menu item is not a transition, so it would be bad design to treat it as such.
*)
    | Menu_Item_Selected item ->
        do notify_menu_selection item.name item.value
        is_visible, Cmd.none

(* Component *)

[<ReactComponent>]
let Menu
    (props : {| expose : IRefValue<I_Menu> |},
    notify_transition_complete : int<command_queue_item_id> -> unit,
    notify_menu_selection : string -> int -> unit)
    : ReactElement =

    let menu_data_1 = React.useRef None
    let is_visible, dispatch = React.useElmish ((false, Cmd.none), update notify_transition_complete notify_menu_selection menu_data_1, [||])
    let is_visible_ref = React.useRef is_visible
    do is_visible_ref.current <- is_visible

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Menu with
                member _.show
                    (menu_data_2 : Menu_Data)
                    (is_notify_transition_complete : bool)
                    (command_queue_item_id_2 : int<command_queue_item_id> option)
                    : unit =

                    dispatch <| Show {
                        data = menu_data_2
                        is_notify_transition_complete = is_notify_transition_complete
                        command_queue_item_id = command_queue_item_id_2
                    }
                member _.hide
                    (is_notify_transition_complete : bool)
                    (command_queue_item_id_2 : int<command_queue_item_id> option)
                    : unit =

                    dispatch <| Hide {
                        is_notify_transition_complete = is_notify_transition_complete
                        command_queue_item_id = command_queue_item_id_2
                    }
                member _.is_visible (): bool = is_visible_ref.current
                member _.get_state () = get_state is_visible_ref menu_data_1
                member _.set_state (saved_state : Menu_Saveable_State) = set_state saved_state dispatch
        }
    )

(* Render *)

    view is_visible_ref menu_data_1 dispatch
