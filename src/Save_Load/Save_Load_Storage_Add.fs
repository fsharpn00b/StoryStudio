module Save_Load_Storage_Add
// DateTime
open System

// console, window
open Browser.Dom
// JS
open Fable.Core
// ? operator
open Fable.Core.JsInterop

open Log
open Save_Load_Helpers
open Save_Load_Storage_Helpers
open Save_Load_Types
open Utilities

(* See:
https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API/Using_IndexedDB
(end)
*)

(* Debug *)

let debug_module_name = "Save_Load_Storage_Add"

let private debug : log_function = debug debug_module_name
let private warn
    (function_name : string)
    (alert : bool)
    (message : string)
    (data : Error_Data)
    : unit =
    warn debug_module_name function_name alert $"{message} {warn_recommendation}" data
let private error : error_function = error debug_module_name

(* Functions - add saved games to storage *)

(* See Save_Load_Storage_Update_Delete.fs for create_new_saved_game (). *)

let private add_saved_game_to_storage_2
    (database_configuration : Database_Configuration)
    (saved_game : New_Saved_Game)
    : JS.Promise<Result<unit, string * Error_Data>> =

    Promise.create (fun resolve _ ->
        let request_1 = open_db database_configuration

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (database_configuration.store_name, "readwrite")
            let store = tx?objectStore database_configuration.store_name
            let request_2 = add_saved_game_to_storage_3 store saved_game

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

let add_saved_game_to_storage_1
    (database_configuration : Database_Configuration)
    (saved_game_name : string)
    (runner_saveable_state_json : string)
    (dispatch : Save_Load_Message -> unit)
    : unit =

    promise {
        try
            let! result_1 = get_canvas true
            match result_1 with

            | Error (message, error_data) ->
                warn "add_saved_game_to_storage_1" true message (["saved_game_name", saved_game_name] @ error_data)

            | Ok canvas ->
                match downscale_screenshot canvas screenshot_max_width screenshot_mime_type screenshot_encoder_options with

                | Error (message, error_data) ->
                    warn "add_saved_game_to_storage_1" true message (["saved_game_name", saved_game_name] @ error_data)

                | Ok screenshot ->
                    let! result_2 = add_saved_game_to_storage_2 database_configuration {
                        name = saved_game_name
                        screenshot = screenshot
                        timestamp = DateTime.UtcNow
                        runner_saveable_state_json = runner_saveable_state_json
                    }
                    
                    match result_2 with

                    | Ok () ->
(* Delay before we hide the save/load game screen, so the mouse click to select the saved game does not also cause us to call Runner_Queue.run (). For now, return the state unchanged. *)
                        do window.setTimeout ((fun () ->
                            dispatch <| Hide
                        ), int hide_save_load_screen_delay_time) |> ignore

                    | Error (message, error_data) ->
                        warn "add_saved_game_to_storage_1" true message (["saved_game_name", saved_game_name] @ error_data)

        with e ->
            warn "add_saved_game_to_storage_1" true "Unknown error." ["saved_game_name", saved_game_name; "error", e]
    } |> Promise.iter ignore

(* When we import saved games into storage, we do not clear the existing saved games, and we use the add function (see Save_Load_Storage_Helpers.add_saved_game_to_storage_2 ()), so ID collisions should not be an issue.
*)
let add_saved_games_to_storage
    (database_configuration : Database_Configuration)
(* Previously we took a New_Saved_Game list. We now take an Existing_Saved_Game list because we need to distinguish quicksave and autosave from other saved games. *)
    (saved_games : Existing_Saved_Game list)
    : JS.Promise<Result<unit, string * Error_Data>> =

    Promise.create (fun resolve _ ->
        let request_1 = open_db database_configuration
        let mutable settled = false

        let settle value =
            if not settled then
                settled <- true
                resolve value

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (database_configuration.store_name, "readwrite")
            let store = tx?objectStore database_configuration.store_name

            tx?oncomplete <- (fun _ ->
                settle (Ok ())
            )
            tx?onerror <- (fun ex ->
                settle (Error ("Failed to store saved games.", ["error", ex]))
            )
            tx?onabort <- (fun ex ->
                settle (Error ("Transaction aborted while storing saved games.", ["error", ex]))
            )

            do saved_games |> List.iter (fun saved_game ->
                let request_2 =
                    if saved_game.id = quicksave_record_id then
                        add_autosave_or_quicksave_to_storage_3 store saved_game.runner_saveable_state_json saved_game.screenshot Quicksave
                    else if saved_game.id = autosave_record_id then
                        add_autosave_or_quicksave_to_storage_3 store saved_game.runner_saveable_state_json saved_game.screenshot Autosave
                    else
                        add_saved_game_to_storage_3 store {
(* If this is not a autosave or quicksave, discard the ID. *)
                            name = saved_game.name
                            screenshot = saved_game.screenshot
                            timestamp = saved_game.timestamp
                            runner_saveable_state_json = saved_game.runner_saveable_state_json
                        }

(* We do not handle request_2?onsuccess because we handle tx?oncomplete instead. *)

(* We do not alert here because (1) we alert in the caller, Save_Load_File.import_saved_game_or_games_from_file () and (2) this might fire multiple times. This is just so we can record the name of each saved game that failed to store. *)
                request_2?onerror <- (fun ex ->
                    warn "add_saved_games_to_storage" false "Failed to store saved game." (["saved_game_name", saved_game.name] @ ["error", ex])
                )
            )
        )

        request_1?onerror <- (fun ex ->
            settle <| Error ("Failed to open database.", ["error", ex])
        )
    )

let add_autosave_or_quicksave_to_storage_2
    (database_configuration : Database_Configuration)
    (screenshot : string)
    (runner_saveable_state : string)
    (autosave_or_quicksave : Autosave_or_Quicksave)
    : JS.Promise<Result<unit, string * Error_Data>> =

    Promise.create (fun resolve _ ->
        let request_1 = open_db database_configuration

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (database_configuration.store_name, "readwrite")
            let store = tx?objectStore database_configuration.store_name

            let request_2 = add_autosave_or_quicksave_to_storage_3 store runner_saveable_state screenshot autosave_or_quicksave

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

let add_autosave_or_quicksave_to_storage_1
    (database_configuration : Database_Configuration)
    (runner_saveable_state_json : string)
    (autosave_or_quicksave : Autosave_or_Quicksave)
    : unit =

    promise {
        try
            let! result_1 = get_canvas true
            match result_1 with

            | Error (message, error_data) ->
                warn "add_autosave_or_quicksave_to_storage_1" true message (["autosave_or_quicksave", autosave_or_quicksave] @ error_data)

            | Ok canvas ->
                match downscale_screenshot canvas screenshot_max_width screenshot_mime_type screenshot_encoder_options with

                | Error (message, error_data) ->
                    warn "add_autosave_or_quicksave_to_storage_1" true message (["autosave_or_quicksave", autosave_or_quicksave] @ error_data)

                | Ok screenshot ->
                    let! result_2 = add_autosave_or_quicksave_to_storage_2 database_configuration screenshot runner_saveable_state_json autosave_or_quicksave
                    match result_2 with

                    | Ok () -> ()

                    | Error (message, error_data) ->
(* Do not show an alert for an autosave error. *)
                        warn "add_autosave_or_quicksave_to_storage_1" (match autosave_or_quicksave with | Quicksave -> true | Autosave -> false) message (["autosave_or_quicksave", autosave_or_quicksave] @ error_data)

        with e ->
(* Do not show an alert for an autosave error. *)
            warn "add_autosave_or_quicksave_to_storage_1" (match autosave_or_quicksave with | Quicksave -> true | Autosave -> false) "Unknown error." ["autosave_or_quicksave", autosave_or_quicksave; "error", e]

    } |> Promise.iter ignore
