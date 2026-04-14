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

(* TODO1 #save We cannot open the save/load screen when there is no background, such as when the author uses the fade_out_all command, because we try to take a screenshot in preparation for the user saving or exporting the game. We should add a try/catch and warning around the screenshot function.

Error:
Uncaught (in promise) DOMException: Failed to execute 'drawImage' on 'CanvasRenderingContext2D': The image argument is a canvas element with a width or height of 0.
at downscale_screenshot (http://localhost:5173/Save_Load/Save_Load_Storage_Helpers.fs.js:49:9)
at http://localhost:5173/Save_Load/Save_Load.fs.js:35:23
*)

let show
    (dispatch : Save_Load_Message -> unit)
    (action : Saved_Game_Action)
    (runner_saveable_state_json : string)
    : unit =

    promise {
        try
            let! usage = get_usage ()
            let! saved_games = get_saved_game_display_data_from_storage ()
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
    (show_game_paused_notification : unit -> unit)
    (message : Save_Load_Message)
    (state_1 : Save_Load_State)
    : Save_Load_State * Cmd<Save_Load_Message> =

(* TODO2 It would be good to distinguish between (1) messages sent by user commands (such as Show) and (2) messages sent by tasks completing (such as Message_Load_Game). Ideally, the functions that do the work (such as add_saved_game_to_storage_1 ()) should be called before we get here. For either 1 or 2, though, the purpose of these message handlers is to update the state and consequently the view.
*)
    match message with

    | Redraw ->
        match state_1 with
        | Visible state_2 ->
            state_1, Cmd.ofEffect (fun dispatch -> show dispatch state_2.action state_2.runner_saveable_state_json)
        | Hidden -> state_1, Cmd.none

    | Show data ->
        Visible {
            action = data.action
            runner_saveable_state_json = data.runner_saveable_state_json
            saved_games = data.saved_games
            usage = data.usage
        }, Cmd.none

    | Hide ->
(* If the player loads, saves, imports, or exports a saved game using a hotkey, we might receive a Hide message even when the save/load screen is already hidden. In that case, we do not want to show the game paused notification. *)
        match state_1 with
        | Visible _ -> do show_game_paused_notification ()
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
    load_game : Runner_Saveable_State -> unit,
    show_game_paused_notification : unit -> unit,
    redraw_command_menu : unit -> unit
    )
    : ReactElement =

(* State *)

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update show_game_paused_notification, [||])
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
                member _.show (action : Saved_Game_Action) (runner_saveable_state_json : string) = show dispatch action runner_saveable_state_json
                member _.export_current_game_to_file (runner_saveable_state_json : string) = export_current_game_to_file runner_saveable_state_json 
                member _.import_current_game_from_file () =
                    open_read_file_dialog (import_saved_game_from_file load_game dispatch)
                member _.download_screenshot () : unit = download_screenshot_1 ()
                member _.quicksave_or_autosave (runner_saveable_state_json : string) (quicksave_or_autosave : Quicksave_Or_Autosave) = add_quicksave_or_autosave_to_storage_1 runner_saveable_state_json quicksave_or_autosave
                member _.hide () = dispatch <| Hide
                member _.switch (action : Saved_Game_Action) = dispatch <| Switch action
                member _.is_visible (): bool = is_visible state_ref
        }
    )

(* Render *)

    view element_ref state_ref load_game dispatch
