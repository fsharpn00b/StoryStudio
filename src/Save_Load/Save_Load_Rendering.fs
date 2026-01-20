module Save_Load_Rendering

// DateTime
open System

// console, window
open Browser.Dom
// HTMLElement
open Browser.Types
open Feliz

open Save_Load_Storage
open Save_Load_Storage_Helpers
open Save_Load_Types
open Units_Of_Measure
open Utilities

(* Helper functions - rendering *)

let private handle_save_new
    (state : Save_Load_Show_Data)
    (dispatch : Save_Load_Message -> unit)
    : unit =

    match window.prompt ("Enter save name:", get_current_timestamp ()) with
    | null -> ()
    | save_name ->
        dispatch <| Message_Save_New_Game {
            name = save_name
            timestamp = DateTime.UtcNow
            screenshot = state.screenshot
            game_state = state.current_game_state
        }

let private handle_slot_click
    (saved_game_id : int<saved_game_id>)
    (existing_saved_game_name : string)
    (state : Save_Load_Show_Data)
    (dispatch : Save_Load_Message -> unit)
    : unit =

    match state.action with

    | Save_Game ->
        if window.confirm $"Overwrite saved game '{existing_saved_game_name}'?" then
(* We dispatch a message because we also need to update the view to hide the saved games screen. *)
            dispatch <| Message_Save_Existing_Game {
                id = saved_game_id
                name = existing_saved_game_name
                screenshot = state.screenshot
                timestamp = DateTime.UtcNow
                game_state = state.current_game_state
            }

    | Load_Game ->
        if window.confirm $"Load save '{existing_saved_game_name}'? Current progress will be lost." then
(* We dispatch a message because we also need to update the view to remove the deleted saved game. *)
            get_saved_game_from_storage saved_game_id |> Promise.iter (fun saved_game_state -> dispatch <| Message_Load_Game saved_game_state)

    | Delete_Game ->
        if window.confirm $"Delete save '{existing_saved_game_name}'? This CANNOT be undone!" then
(* We dispatch a message because we also need to update the view to remove the deleted saved game. *)
            dispatch <| Message_Delete_Game saved_game_id

(* This is for debugging. *)
let private view_saved_game_grid_test_overflow
    (state : IRefValue<Save_Load_State>)
    (dispatch : Save_Load_Message -> unit)
    : ReactElement seq =

    let screenshot = "data:image/jpeg;base64,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"

    [1..30]
        |> Seq.map (fun _ ->
            Html.div [
                prop.className "saved_game_slot"
                prop.children [
                    Html.img [ prop.src screenshot ]
                    Html.div [ prop.text $"TEST{Environment.NewLine}" ]
                ]
            ]
        )

let private view_saved_game_grid
    (state : Save_Load_Show_Data)
    (dispatch : Save_Load_Message -> unit)
    : ReactElement seq =

    state.saved_games
        |> Seq.map (fun kv ->
            Html.div [
                prop.className "saved_game_slot"
                prop.onClick (fun event ->
                    do
                        event.stopPropagation ()
                        handle_slot_click kv.Key kv.Value.name state dispatch
                )
                prop.children [
                    Html.img [ prop.src kv.Value.screenshot ]
                    Html.div [ prop.text $"{kv.Value.name}{Environment.NewLine}" ]
                ]
            ]
        )

(* Main functions - public *)

let view
    (element_ref : IRefValue<HTMLElement option>)
    (state_1 : IRefValue<Save_Load_State>)
    (dispatch : Save_Load_Message -> unit)
    : ReactElement =

    match state_1.current with

    | Hidden -> Html.none

    | Visible state_2 ->
        Html.div [
(* Make sure this screen can receive focus. This is not strictly needed if we are not stopping propagation of key down events, but we are leaving it here for now in case it is useful later. *)
            prop.ref element_ref
            prop.tabIndex 0
(* Unlike in the configuration screen, we do not stop the propagation of key down events.
window.prompt () seems to intercept key down events before window receives them, so we do not have to worry about the player triggering key down events while entering a save game name.
*)
(* Prevent a mouse click from calling Runner.run (). This does not prevent the player from selecting a saved game to load, save over, or delete. *)
            prop.onClick (fun event -> do event.stopPropagation ())
(* Prevent a mouse wheel scroll event from calling Runner.undo ()/redo (). *)
            prop.onWheel (fun event -> do event.stopPropagation ())

            prop.id "save_load_screen"
            prop.style [style.zIndex save_load_screen_z_index]
            prop.children [
                Html.div [
                    prop.id "save_load_header"
                    prop.children [
                        Html.h3 [
                            let text =
                                match state_2.action with
                                | Save_Game -> "Save Game"
                                | Load_Game -> "Load Game"
                                | Delete_Game -> "Delete Game" 
                            prop.text $"{text} (Escape to exit)"
                        ]
                        Html.div [
                            prop.text $"Usage: %.2f{state_2.usage.usage} MB of %.2f{state_2.usage.quota} MB (%.2f{state_2.usage.usage / state_2.usage.quota * 100.0} %%)"
                        ]
                    ]
                ]
                Html.div [
                    prop.id "save_load_grid"
                    prop.children (view_saved_game_grid state_2 dispatch)
                ]
                Html.div [
                    prop.className "controls"
                    prop.children [
                        match state_2.action with
                        | Save_Game ->
                            Html.button [
                                prop.text "New Save"
                                prop.onClick (fun event ->
                                    do
                                        event.stopPropagation ()
                                        handle_save_new state_2 dispatch
                                )
                            ]
                        | Delete_Game ->
                            Html.button [
                                prop.text "Delete All"
                                prop.onClick (fun event ->
                                    do event.stopPropagation ()
                                    if window.confirm "Delete all saved games? This CANNOT be undone!" then
                                        do dispatch <| Message_Delete_All_Games
                                )
                            ]
                        | _ -> Html.none
                        Html.button [
                            prop.text "Exit"
                            prop.onClick (fun event ->
                                do
                                    event.stopPropagation ()
                                    dispatch Hide
                            )
                        ]
                    ]
                ]
            ]
        ]
