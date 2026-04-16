module Save_Load_Storage_Update_Delete

// DateTime
open System

// console, window
open Browser.Dom
// JS
open Fable.Core
// ? operator
open Fable.Core.JsInterop

open Log
open Notification_Types
open Save_Load_Helpers
open Save_Load_Storage_Add
open Save_Load_Storage_Helpers
open Save_Load_Types
open Save_Load_Validation
open Units_Of_Measure
open Utilities

(* See:
https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API/Using_IndexedDB
(end)
*)

(* Debug *)

let debug_module_name = "Save_Load_Storage_Update_Delete"

let private debug : log_function = debug debug_module_name
let private warn
    (function_name : string)
    (alert : bool)
    (message : string)
    (data : Error_Data)
    : unit =
    warn debug_module_name function_name alert $"{message} {warn_recommendation}" data
let private error : error_function = error debug_module_name

(* Functions - update saved games in storage *)

let private overwrite_saved_game_in_storage_2
    (database_configuration : Database_Configuration)
    (saved_game : Existing_Saved_Game)
    : JS.Promise<Result<unit, string * Error_Data>> =

    Promise.create (fun resolve _ ->
        let request_1 = open_db database_configuration

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (database_configuration.store_name, "readwrite")
            let store = tx?objectStore database_configuration.store_name

            let request_2 = overwrite_saved_game_in_storage_3 store saved_game

            request_2?onsuccess <- (fun _ ->
                do resolve <| Ok ()
            )

            request_2?onerror <- (fun ex ->
                do resolve <| Error ("Failed to store saved game.", ["error", ex])
            )
        )

        request_1?onerror <- (fun ex ->
            do resolve <| Error ("Failed to open database.", ["error", ex])
        )
    )

let overwrite_existing_game_in_storage_1
    (database_configuration : Database_Configuration)
    (existing_saved_game_id : int<saved_game_id>)
    (existing_saved_game_name : string)
    (runner_saveable_state_json : string)
    (dispatch : Save_Load_Message -> unit)
    : unit =

    let error_data_1 = ["existing_saved_game_id", existing_saved_game_id :> obj; "existing_saved_game_name", existing_saved_game_name]

    if window.confirm $"Overwrite saved game '{existing_saved_game_name}'?" then
        promise {
            try
                let! result_1 = get_canvas true
                match result_1 with

                | Error (message, error_data) ->
                    do warn "overwrite_existing_game_in_storage_1" true message (error_data_1 @ error_data)

                | Ok canvas ->
                    match downscale_screenshot canvas screenshot_max_width screenshot_mime_type screenshot_encoder_options with

                    | Error (message, error_data) ->
                        do warn "overwrite_existing_game_in_storage_1" true message (error_data_1 @ error_data)

                    | Ok screenshot ->
                        let! result_2 = overwrite_saved_game_in_storage_2 database_configuration {
                            id = existing_saved_game_id
                            name = existing_saved_game_name
                            screenshot = screenshot
                            timestamp = DateTime.UtcNow
                            runner_saveable_state_json = runner_saveable_state_json
                        }

                        match result_2 with

                        | Ok () ->
(* Delay before we hide the save/load game screen, so the mouse click to select the saved game does not also cause us to call Runner_Queue.run (). For now, return the state unchanged. *)
                            do window.setTimeout ((fun () ->
                                Save_Complete |> Some |> Hide |> dispatch
                            ), int hide_save_load_screen_delay_time) |> ignore

                        | Error (message, error_data_2) ->
                            do warn "overwrite_existing_game_in_storage_1" true message (error_data_1 @ error_data_2)

            with e ->
                do warn "overwrite_existing_game_in_storage_1" true "Unknown error." (error_data_1 @ ["error", e])

        } |> Promise.iter ignore

(* This must be defined after overwrite_existing_game_in_storage_1 (), so it cannot go in the add saved games to storage section. *)
let create_new_saved_game
    (database_configuration : Database_Configuration)
    (state : Save_Load_Show_Data)
    (dispatch : Save_Load_Message -> unit)
    : unit =

    match window.prompt ("Enter save name:", get_current_timestamp ()) with

    | null -> ()

    | save_name ->
        let validate_saved_game_name_result = validate_saved_game_name save_name
        match validate_saved_game_name_result with

        | Error (message, data) ->
            do warn "handle_save_new" true message data
            
        | Ok () ->
            match state.saved_games |> Seq.tryFind (fun kv -> kv.Value.name = save_name) with

            | Some saved_game ->
                do overwrite_existing_game_in_storage_1 database_configuration saved_game.Key save_name state.runner_saveable_state_json dispatch

            | None ->
                do add_saved_game_to_storage_1 database_configuration save_name state.runner_saveable_state_json dispatch

(* Functions - delete saved games from storage *)

let private delete_saved_game_from_storage_2
    (database_configuration : Database_Configuration)
    (saved_game_id : int<saved_game_id>)
    : JS.Promise<Result<unit, string * Error_Data>> =

    Promise.create (fun resolve _ ->
        let request_1 = open_db database_configuration

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (database_configuration.store_name, "readwrite")
            let store = tx?objectStore database_configuration.store_name
            let request_2 = store?delete saved_game_id

            request_2?onsuccess <- (fun _ ->
                do resolve <| Ok ()
            )

            request_2?onerror <- (fun ex ->
                do resolve <| Error ("Failed to delete saved game.", ["error", ex])
            )
        )

        request_1?onerror <- (fun ex ->
            do resolve <| Error ("Failed to open database.", ["error", ex])
        )
    )

let delete_saved_game_from_storage_1
    (database_configuration : Database_Configuration)
    (existing_saved_game_id : int<saved_game_id>)
    (existing_saved_game_name : string)
    (dispatch : Save_Load_Message -> unit)
    : unit =

    let error_data_1 = ["existing_saved_game_id", existing_saved_game_id :> obj; "existing_saved_game_name", existing_saved_game_name]

    if window.confirm $"Delete save '{existing_saved_game_name}'? This CANNOT be undone!" then

        promise {
            try
                let! result = delete_saved_game_from_storage_2 database_configuration existing_saved_game_id

                match result with
                
                | Ok () ->
                    do dispatch <| Redraw

                | Error (message, error_data_2) ->
                    do warn "delete_saved_game_from_storage_1" true message (error_data_1 @ error_data_2)

            with e ->
                do warn "delete_saved_game_from_storage_1" true "Unknown error." (error_data_1 @ ["error", e])

        } |> Promise.iter ignore

let private delete_all_saved_games_from_storage_2
    (database_configuration : Database_Configuration)
    : JS.Promise<Result<unit, string * Error_Data>> =

    Promise.create (fun resolve _ ->

        let request_1 = open_db database_configuration

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (database_configuration.store_name, "readwrite")
            let store = tx?objectStore database_configuration.store_name
            let request_2 = store?clear ()

            request_2?onsuccess <- (fun _ ->
                do resolve <| Ok ()
            )

            request_2?onerror <- (fun ex ->
                do resolve <| Error ("Failed to delete saved games.", ["error", ex])
            )
        )

        request_1?onerror <- (fun ex ->
            do resolve <| Error ("Failed to open database.", ["error", ex])
        )
    )

let delete_all_saved_games_from_storage_1
    (database_configuration : Database_Configuration)
    (dispatch : Save_Load_Message -> unit)
    : unit =

    if window.confirm "Delete all saved games? This CANNOT be undone!" then
        promise {
            try
                let! result = delete_all_saved_games_from_storage_2 database_configuration
                match result with

                | Ok () ->
                    do dispatch <| Redraw

                | Error (message, error_data) ->
                    do warn "delete_all_saved_games_from_storage_1" true message error_data

            with e ->
                do warn "delete_all_saved_games_from_storage_1" true "Unknown error." ["error", e]
        } |> Promise.iter ignore
