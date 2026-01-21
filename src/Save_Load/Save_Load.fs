module Save_Load

// DateTime
open System

// console, window
open Browser.Dom
open Elmish
// ? operator
open Fable.Core.JsInterop
open Feliz
open Feliz.UseElmish

open Log
open Save_Load_Rendering
open Save_Load_Storage
open Save_Load_Storage_Helpers
open Save_Load_Types
open Utilities

(* Debug *)

let debug_module_name = "Save_Load"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions - interface *)

let show
    (dispatch : Save_Load_Message -> unit)
    (action : Saved_Game_Action)
    (current_game_state : string)
    : unit =

    promise {
        let! canvas = get_canvas ()
        let! usage = get_usage ()
        let! saved_games = get_saved_game_display_data_from_storage ()
        return {|
            screenshot = downscale_screenshot canvas screenshot_max_width screenshot_mime_type screenshot_encoder_options
            usage = usage
            saved_games = saved_games
        |}
    } |> Promise.iter (fun results ->
        dispatch <| Show {
            action = action
            current_game_state = current_game_state
            screenshot = results.screenshot
            saved_games = results.saved_games
            usage = results.usage
        }
    )

let download_screenshot_1 () : unit =

    promise {
        let! canvas = get_canvas ()
        return canvas
    } |> Promise.iter (fun canvas ->
        download_screenshot_2 canvas "image/png"
    )

(* Main functions - state *)

let is_visible (state_ref : IRefValue<Save_Load_State>) : bool =
    match state_ref.current with
    | Visible _ -> true
    | Hidden -> false

let private update
(* load_game is Runner_State.load_game (), closed over runner_component_interfaces, history, and queue. All it needs is the saved game state. *)
    (load_game : string -> unit)
    (show_game_paused_notification : unit -> unit)
    (message : Save_Load_Message)
    (state_1 : Save_Load_State)
    : Save_Load_State * Cmd<Save_Load_Message> =

(* TODO2 It would be good to distinguish between (1) messages sent by user commands (such as Show) and (2) messages sent by tasks completing (such as Message_Load_Game). Ideally, the functions that do the work (such as add_saved_game_to_storage_1 ()) should be called before we get here. For either 1 or 2, though, the purpose of these message handlers is to update the state and consequently the view.
*)
    match message with

    | Show data ->
        Visible {
            action = data.action
            current_game_state = data.current_game_state
            screenshot = data.screenshot
            saved_games = data.saved_games
            usage = data.usage
        }, Cmd.none

    | Hide ->
        do show_game_paused_notification ()
        Hidden, Cmd.none

    | Switch action ->
        match state_1 with
        | Hidden -> error "update" "Unexpected state." ["state", state_1] |> invalidOp
        | Visible state_2 ->
            if action = state_2.action then state_1, Cmd.none
            else Visible { state_2 with action = action }, Cmd.none

    | Message_Load_Game game_state ->
        do load_game game_state
(* Delay before we hide the save/load game screen, so the mouse click to select the saved game does not also cause us to call Runner_Queue.run (). For now, return the state unchanged. *)
        state_1, Cmd.ofEffect (fun dispatch ->
            do window.setTimeout ((fun () ->
                dispatch <| Hide
            ), int hide_save_load_screen_delay_time) |> ignore
        )

    | Message_Save_New_Game saved_game ->
        do add_saved_game_to_storage_1 saved_game
        state_1, Cmd.ofEffect (fun dispatch ->
            do window.setTimeout ((fun () ->
                dispatch <| Hide
            ), int hide_save_load_screen_delay_time) |> ignore
        )

    | Message_Save_Existing_Game saved_game ->
        do overwrite_saved_game_in_storage_1 saved_game
        state_1, Cmd.ofEffect (fun dispatch ->
            do window.setTimeout ((fun () ->
                dispatch <| Hide
            ), int hide_save_load_screen_delay_time) |> ignore
        )

    | Message_Delete_Game saved_game_id ->
        match state_1 with
        | Hidden -> error "update/Message_Delete_Game" "Unexpected state." ["state", state_1] |> invalidOp
        | Visible state_2 ->
            do delete_saved_game_from_storage saved_game_id
            Visible {
                state_2 with
                    saved_games = state_2.saved_games.Remove saved_game_id
            }, Cmd.none

    | Message_Delete_All_Games ->
        match state_1 with
        | Hidden -> error "update/Message_Delete_All_Games" "Unexpected state." ["state", state_1] |> invalidOp
        | Visible state_2 ->
            do delete_all_saved_games_from_storage ()
            Visible {
                state_2 with
                    saved_games = Map.empty
            }, Cmd.none

(* Component *)

[<ReactComponent>]
let Save_Load
    (props : {| expose : IRefValue<I_Save_Load> |},
    load_game : string -> unit,
    show_game_paused_notification : unit -> unit,
    redraw_command_menu : unit -> unit
    )
    : ReactElement =

(* State *)

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update load_game show_game_paused_notification, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

(* Give focus to this component when it is visible. This is so we can prevent mouse click and key down events leaking to the game. *)
    let element_ref = React.useRef None
    React.useEffect((fun () -> match state with | Visible _ -> element_ref.current?focus() | _ -> ()), [| box state |])

(* Notify the command menu when this screen shows or hides itself, so the command menu can enable or disable the appropriate commands.
Unlike Configuration_State, Save_Load_State does not have a simple is_visible flag. Save_Load_State has states Visible and Hidden because it needs to store additional data in the Visible state. As a result we have to depend on the entire Save_Load_State. If we add other fields to Save_Load_State, we might need to add a separate is_visible flag that we can depend on instead, so we do not trigger unneeded redraws of the command menu. *)
    React.useEffect ((fun () -> redraw_command_menu ()), [| box state |])

(* Interface *)

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Save_Load with
(* The save/load screen does not need to notify transition complete. It does not have a Command_Behavior, and should not add to the history or call get_next_command ().

The caller must send the current game state, and we must take a screenshot, in case the player switches from the load screen to the save screen without returning to the game.
*)
                member _.show (action : Saved_Game_Action) (current_game_state : string) = show dispatch action current_game_state
                member _.export_saved_games_from_storage_to_file () = export_saved_games_from_storage_to_file ()
                member _.import_saved_games_from_file_to_storage () = import_saved_games_from_file_to_storage ()
                member _.export_current_game_to_file (current_game_state : string) = export_current_game_to_file current_game_state 
                member _.import_current_game_from_file () = import_current_game_from_file dispatch
                member _.download_screenshot () : unit = download_screenshot_1 ()
                member _.quicksave_or_autosave (current_game_state : string) (quicksave_or_autosave : Quicksave_Or_Autosave) = add_quicksave_or_autosave_to_storage_1 current_game_state quicksave_or_autosave
                member _.hide () = dispatch <| Hide
                member _.switch (action : Saved_Game_Action) = dispatch <| Switch action
                member _.is_visible (): bool = is_visible state_ref
        }
    )

(* Render *)

    view element_ref state_ref dispatch
