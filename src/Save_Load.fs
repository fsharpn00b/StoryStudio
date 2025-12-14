module Save_Load

// DateTime
open System

// navigator, Types
open Browser
(* TODO2 For some reason we do not need to import this for now. *)
// document, window
//open Browser.Dom
// a, Element, HTMLCanvasElement
open Browser.Types
// localStorage
//open Browser.WebStorage
open Elmish
// Import, jsNative
open Fable.Core
// ? operator
open Fable.Core.JsInterop
open Feliz
open Feliz.UseElmish
// Decode, Encode
open Thoth.Json

open Log
open Save_Load_Rendering
open Save_Load_Storage
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
        return {|
            screenshot = downscale_screenshot canvas 160 "image/jpeg" 0.7
            usage = usage
        |}
    } |> Promise.iter (fun results ->
        dispatch <| Show {
            action = action
            current_game_state = current_game_state
            screenshot = results.screenshot
            usage = results.usage
        }
    )

let download_screenshot () : unit =

    promise {
        let! canvas = get_canvas ()
        return canvas
    } |> Promise.iter (fun canvas ->
        download_screenshot_2 canvas "image/png"
    )

(* Main functions - state *)

let private update
    (load_game : string -> unit)
    (message : Save_Load_Message)
    (state : Save_Load_State)
    : Save_Load_State * Cmd<Save_Load_Message> =

    match message with

    | Set_Saved_Games_In_State saved_games ->
        { state with saved_games = saved_games }, Cmd.none

    | Show data ->
        {
            state with
                action = data.action
                current_game_state = data.current_game_state
                screenshot = data.screenshot
                usage = data.usage
                is_visible = true
        }, Cmd.none

    | Hide -> { state with is_visible = false }, Cmd.none

    | Switch action ->
        (if action = state.action then state else { state with action = action }), Cmd.none

    | Message_Load saved_game_name ->
        do load_game saved_game_name
(* Delay before we hide the save/load game screen, so the mouse click on the Load button does not cause us to call Runner_Queue.run (). *)
        state, Cmd.ofEffect (fun dispatch ->
            do window.setTimeout ((fun () ->
                dispatch <| Hide
            ), int hide_save_load_screen_delay_time) |> ignore
        )

    | Message_Save saved_game ->
        let saved_games = state.saved_games.Add (saved_game.name, saved_game)
        set_saved_games_in_storage saved_games
(* Delay before we hide the save/load game screen, so the mouse click on the Load button does not cause us to call Runner_Queue.run (). *)
        { state with saved_games = saved_games }, Cmd.ofEffect (fun dispatch ->
            do window.setTimeout ((fun () ->
                dispatch <| Hide
            ), int hide_save_load_screen_delay_time) |> ignore
        )

    | Message_Delete saved_game_name ->
        let saved_games = state.saved_games.Remove saved_game_name
        set_saved_games_in_storage saved_games
        { state with saved_games = saved_games }, Cmd.none

(* Component *)

[<ReactComponent>]
let Save_Load
    (props : {| expose : IRefValue<I_Save_Load> |},
    load_game : string -> unit)
    : ReactElement =

    let state, dispatch = React.useElmish ((initial_state, Cmd.none), update load_game, [||])
    let state_ref = React.useRef state
    do state_ref.current <- state

    React.useEffectOnce (fun () ->
(* TODO1 For some reason, updating state_ref with the saved games does not work, even though view () uses state_ref rather than state. We must update state itself. To do that, we must dispatch a message.
We need to investigate this because it could also affect other components that use state and state_ref. It seems state is reflected to state_ref, but not the reverse. That is understandable. But it makes no sense that changes to state_ref are ignored.
We could work around this by having state_ref use a set () method that dispatches a message that updates state.
*)
        do get_saved_games_from_storage dispatch
    )

    React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Save_Load with
(* The save/load screen does not need to notify transition complete. It does not have a Command_Behavior, and should not add to the history or call get_next_command ().

The caller must send the current game state, and we must take a screenshot, in case the player switches from the load screen to the save screen without returning to the game.
*)
                member _.show (action : Saved_Game_Action) (current_game_state : string) = show dispatch action current_game_state
                member _.export () = export_saved_games_to_file state_ref.current.saved_games
                member _.import () = import_saved_games_from_file dispatch
                member _.download_screenshot () : unit = download_screenshot ()
                member _.hide () = dispatch <| Hide
                member _.switch (action : Saved_Game_Action) = dispatch <| Switch action
                member _.is_visible (): bool = state_ref.current.is_visible
        }
    )

(* Render *)

    view state_ref dispatch
