module Save_Load_Storage_Load

// console, window
open Browser.Dom
// Import, JS, jsNative
open Fable.Core
// ? operator
open Fable.Core.JsInterop

open Log
open Runner_Types_1
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

let debug_module_name = "Save_Load_Storage_Load"

let private debug : log_function = debug debug_module_name
let private warn
    (function_name : string)
    (alert : bool)
    (message : string)
    (data : (string * obj) list)
    : unit =
    warn debug_module_name function_name alert $"{message} {warn_recommendation}" data
let private error : error_function = error debug_module_name

(* Functions - get saved games from storage *)

(* Fable does not support the Exception.innerException property, so we use a Result instead. *)
let get_saved_game_display_data_from_storage ()
    : JS.Promise<Result<Saved_Games_Display_Data, string * Error_Data>> =

(* indexeddb methods fire asynchronous events onsuccess/onerror.
The boundary between (1) the action we perform in the database and (2) its result can be represented by onsuccess/onerror, Promise, or dispatch ().
We wrap onsuccess/onerror with a Promise, which is just adding another layer. However, it seems it is more idiomatic to return a Promise up the call stack until we reach a function that has a dispatch () function, such as view () or one of its helpers, than to pass the dispatch () function down the call stack.
*)
    Promise.create (fun resolve _ ->
        let request_1 = open_db ()
        let mutable results_1 = Map.empty

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (store_name, "readonly")
            let store = tx?objectStore store_name

            let request_2 = store?openCursor ()
            request_2?onsuccess <- (fun event ->
                let cursor = event?target?result
(* Keep reading the cursor until it returns None. It seems the cursor fires onsuccess/onerror for each record. *)
                match cursor with
                | None -> resolve <| Ok results_1
                | Some results_2 ->
                    do
                        results_1 <- results_1.Add (results_2?key |> LanguagePrimitives.Int32WithMeasure, { name = results_2?value?name; screenshot = results_2?value?screenshot })
(* continue is a reserved word, so we must enclose it with backticks. *)
                        cursor?``continue`` ()
            )

            request_2?onerror <- (fun ex ->
                do resolve <| Error ("Database cursor error.", ["error", ex])
            )
        )

        request_1?onerror <- (fun ex ->
            do resolve <| Error ("Failed to open database.", ["error", ex])
        )
    )

let get_saved_game_from_storage_2
    (saved_game_id : int<saved_game_id>)
    : JS.Promise<Result<string, string * Error_Data>> =

    Promise.create (fun resolve _ ->
        let request_1 = open_db ()

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (store_name, "readonly")
            let store = tx?objectStore store_name

            let request_2 = store?get saved_game_id
            request_2?onsuccess <- (fun _ ->
                match request_2?result with
                | None -> do resolve <| Error ("Saved game not found.", [])
                | Some result_2 -> do resolve <| Ok result_2?runner_saveable_state_json
            )

            request_2?onerror <- (fun e ->
                do resolve <| Error ("Failed to get saved game.", ["error", e])
            )
        )

        request_1?onerror <- (fun e ->
            do resolve <| Error ("Failed to open database.", ["error", e])
        )
    )

let get_saved_game_from_storage_1
    (existing_saved_game_id : int<saved_game_id>)
    (existing_saved_game_name : string)
(* load_game is Runner_State.load_game (), closed over runner_component_interfaces, history, and queue. All it needs is the saved game state. *)
    (load_game : Runner_Saveable_State -> unit)
    (dispatch : Save_Load_Message -> unit)
    : unit =

    let error_data_1 = ["saved_game_id", existing_saved_game_id :> obj; "saved_game_name", existing_saved_game_name]

(* Save_Load_Storage.get_saved_game_from_storage () returns a serialized Runner_Saveable_State rather than a New_Saved_Game. The former is all we need to load the game. The latter also contains the ID, name, screenshot, and timestamp, which we only need to show all saved games in the save/load screen.
*)
    promise {
        try
            let! result = get_saved_game_from_storage_2 existing_saved_game_id
            match result with

            | Ok runner_saveable_state_json ->

                match validate_and_parse_runner_saveable_state runner_saveable_state_json with
                
                | Ok runner_saveable_state ->

                    if window.confirm $"Load save '{existing_saved_game_name}'? Current progress will be lost." then
                        do
                            load_game runner_saveable_state
(* Delay before we hide the save/load game screen, so the mouse click to select the saved game does not also cause us to call Runner_Queue.run (). For now, return the state unchanged. *)
                            window.setTimeout ((fun () ->
                                dispatch <| Hide
                            ), int hide_save_load_screen_delay_time) |> ignore

                | Error (message, error_data_2) ->

                    warn "get_saved_game_from_storage_1" true message (error_data_1 @ error_data_2)

            | Error (message, error_data_2) ->

                warn "get_saved_game_from_storage_1" true message (error_data_1 @ error_data_2)

        with e ->
            warn "get_saved_game_from_storage_1" true "Unknown error." (error_data_1 @ ["error", e])
    } |> Promise.iter ignore

let get_all_saved_games_from_storage ()
    : JS.Promise<Result<Existing_Saved_Game list, string * Error_Data>> =

    Promise.create (fun resolve _ ->
        let request_1 = open_db ()
        let mutable results_1 = []

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (store_name, "readonly")
            let store = tx?objectStore store_name

            let request_2 = store?openCursor ()

            request_2?onsuccess <- (fun event ->
                let cursor = event?target?result
(* Keep reading the cursor until it returns None. It seems the cursor fires onsuccess/onerror for each record. *)
                match cursor with
                | None -> resolve <| Ok results_1
                | Some results_2 ->
                    let saved_game = {
                        id = results_2?key
                        name = results_2?value?name
                        timestamp = results_2?value?timestamp
                        screenshot = results_2?value?screenshot
                        runner_saveable_state_json = results_2?value?runner_saveable_state_json 
                    }
                    do
                        results_1 <- saved_game :: results_1
(* continue is a reserved word, so we must enclose it with backticks. *)
                        cursor?``continue`` ()
            )

            request_2?onerror <- (fun ex ->
                do resolve <| Error ("Database cursor error.", ["error", ex])
            )
        )

        request_1?onerror <- (fun ex ->
            do resolve <| Error ("Failed to open database.", ["error", ex])
        )
    )
