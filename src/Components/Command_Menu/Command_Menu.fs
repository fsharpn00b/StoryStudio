module Command_Menu

// DateTime
open System

open Elmish
open Feliz
open Feliz.UseElmish

open Log
open Utilities

(* Debug *)

let debug_module_name = "Command_Menu"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Types *)

type Command_Menu_Commands = {
    show_or_hide_configuration_screen : unit -> unit
    show_save_game_screen : unit -> unit
    show_load_game_screen : unit -> unit
    can_undo : unit -> bool
    can_redo : unit -> bool
    undo : unit -> unit
    redo : unit -> unit
}

type private State = {
    is_visible : bool
}

type private Message =
    | Show
    | Hide

(* Interfaces *)

type I_Command_Menu =
    abstract member show : unit -> unit
    abstract member hide : unit -> unit
    abstract member is_visible : unit -> bool
    abstract member redraw : unit -> unit

(* Consts *)

let private initial_state = {
    is_visible = true
}

(* Main functions - rendering *)

let private view
    (commands : Command_Menu_Commands)
    (state : IRefValue<State>)
    (dispatch : Message -> unit)
    : ReactElement =

    let can_undo = commands.can_undo ()
    let can_redo = commands.can_redo ()

    if state.current.is_visible then
        Html.div [
            prop.id "command_menu_container"
            prop.style [style.zIndex command_menu_z_index]
            prop.children [
                Html.div [
                    prop.id "command_menu"
                    prop.children [
                        Html.label [
                            prop.text "Save"
                            prop.onClick (fun event ->
                                do
                                    event.stopPropagation ()
                                    commands.show_save_game_screen ()
                            )
                        ]
                        Html.label [
                            prop.text "Load"
                            prop.onClick (fun event ->
                                do
                                    event.stopPropagation ()
                                    commands.show_load_game_screen ()
                            )
                        ]
                        Html.label [
(* This key is needed to make sure the label re-renders when the value of can_undo changes. *)
                            prop.key $"undo_{can_undo}"
                            prop.text "Undo"
                            prop.onClick (fun event ->
                                do
                                    event.stopPropagation ()
                                    if can_undo then commands.undo ()
                            )
                            let color = if can_undo then "white" else "gray"
                            prop.style <| [style.color color]
                        ]
                        Html.label [
(* This key is needed to make sure the label re-renders when the value of can_redo changes. *)
                            prop.key $"redo_{can_redo}"
                            prop.text "Redo"
                            prop.onClick (fun event ->
                                do
                                    event.stopPropagation ()
                                    if can_redo then commands.redo ()
                            )
                            let color = if can_redo then "white" else "gray"
                            prop.style <| [style.color color]
                        ]
                        Html.label [
                            prop.text "Configuration"
                            prop.onClick (fun event ->
                                do
                                    event.stopPropagation ()
                                    commands.show_or_hide_configuration_screen ()
                            )
                        ]
                    ]
                ]
            ]
        ]
    else Html.none

(* Main functions - state *)

let private update
    (message : Message)
    (state : State)
    : State * Cmd<Message> =

    match message with

    | Show ->
        {
            state with
                is_visible = true
        }, Cmd.none

    | Hide -> { state with is_visible = false }, Cmd.none

(* Component *)

[<ReactComponent>]
let Command_Menu
    (props : {| expose : IRefValue<I_Command_Menu> |},
    commands : Command_Menu_Commands
    )
    : ReactElement =

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

    let _, redraw = React.useReducer((fun s _ -> s + 1), 0)

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Command_Menu with
                member _.show () : unit = dispatch <| Show
                member _.hide () : unit = dispatch <| Hide
                member _.is_visible () : bool = state_ref.current.is_visible
                member _.redraw () : unit = redraw ()
        }
    )

(* Render *)

    view commands state_ref dispatch
