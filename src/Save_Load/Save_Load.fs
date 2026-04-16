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
open Notification_Types
open Runner_Types_1
open Save_Load_File
open Save_Load_Helpers
open Save_Load_Rendering
open Save_Load_Storage_Add
open Save_Load_Storage_Load
open Save_Load_Storage_Helpers
open Save_Load_Types

(* Debug *)

let debug_module_name = "Save_Load"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions - interface *)

let show
    (database_configuration : Database_Configuration)
    (dispatch : Save_Load_Message -> unit)
    (action : Saved_Game_Action)
    (runner_saveable_state_json : string)
    : unit =

    promise {
        try
            let! usage = get_usage ()
            let! saved_games = get_saved_game_display_data_from_storage database_configuration
            match saved_games with

            | Ok saved_games ->
                do dispatch <| Show {
                    action = action
                    runner_saveable_state_json = runner_saveable_state_json
                    saved_games = saved_games
                    usage = usage
                }

            | Error e -> warn "show" true "Error getting saved games and storage usage data." ["error", e]

        with e ->
            warn "show" true "Unknown error." ["error", e]
    } |> Promise.iter ignore

(* Main functions - state *)

let is_visible (state_ref : IRefValue<Save_Load_State>) : bool =
    match state_ref.current with
    | Visible _ -> true
    | Hidden -> false

let private update
    (database_configuration : Database_Configuration)
    (show_pause_notification : Pause_Notification_Type -> unit)
    (message : Save_Load_Message)
    (state_1 : Save_Load_State)
    : Save_Load_State * Cmd<Save_Load_Message> =

(* TODO2 It would be good to distinguish between (1) messages sent by user commands (such as Show) and (2) messages sent by tasks completing (such as Message_Load_Game). Ideally, the functions that do the work (such as add_saved_game_to_storage_1 ()) should be called before we get here. For either 1 or 2, though, the purpose of these message handlers is to update the state and consequently the view.
*)
    match message with

    | Redraw ->
        match state_1 with
        | Visible state_2 ->
            state_1, Cmd.ofEffect (fun dispatch -> show database_configuration dispatch state_2.action state_2.runner_saveable_state_json)
        | Hidden -> state_1, Cmd.none

    | Show data ->
        Visible {
            action = data.action
            runner_saveable_state_json = data.runner_saveable_state_json
            saved_games = data.saved_games
            usage = data.usage
        }, Cmd.none

    | Hide pause_notification_type ->
(* If the player loads, saves, imports, or exports a saved game using a hotkey, we might receive a Hide message even when the save/load screen is already hidden. In that case, we do not want to show the game paused notification. *)
        match state_1 with
        | Visible _ ->
            match pause_notification_type with
            | Some pause_notification_type ->
                do show_pause_notification pause_notification_type
            | None -> ()
        | Hidden -> ()
        Hidden, Cmd.none

    | Switch action ->
        match state_1 with
        | Hidden -> error "update" "Unexpected state." ["state", state_1] |> invalidOp
        | Visible state_2 ->
            if action = state_2.action then state_1, Cmd.none
            else Visible { state_2 with action = action }, Cmd.none

(* Component *)

[<ReactComponent>]
let Save_Load
    (props : {| expose : IRefValue<I_Save_Load> |},
    database_configuration : Database_Configuration,
    load_game : Runner_Saveable_State -> unit,
    show_pause_notification : Pause_Notification_Type -> unit,
    redraw_command_menu : unit -> unit
    )
    : ReactElement =

(* State *)

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update database_configuration show_pause_notification, [||])
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
                member _.show (action : Saved_Game_Action) (runner_saveable_state_json : string) = show database_configuration dispatch action runner_saveable_state_json
                member _.export_current_game_to_file_via_hotkey (runner_saveable_state_json : string) (notify_success : unit -> unit) = export_current_game_to_file runner_saveable_state_json notify_success
                member _.import_current_game_from_file_via_hotkey (notify_success : unit -> unit) =
                    open_read_file_dialog (import_current_game_from_file_1 load_game dispatch notify_success)
                member _.download_screenshot () : unit = download_screenshot_1 ()
(* Do not show a notification for autosave. *)
                member _.autosave_or_quicksave (runner_saveable_state_json : string) (autosave_or_quicksave : Autosave_or_Quicksave) (notify_success : unit -> unit) = add_autosave_or_quicksave_to_storage_1 database_configuration runner_saveable_state_json autosave_or_quicksave notify_success
                member _.hide (pause_notification_type : Pause_Notification_Type option) = dispatch <| Hide pause_notification_type
                member _.switch (action : Saved_Game_Action) = dispatch <| Switch action
                member _.is_visible (): bool = is_visible state_ref
        }
    )

(* Render *)

    view database_configuration element_ref state_ref load_game dispatch
