module Save_Load_Storage_Helpers

// DateTime
open System

// console, navigator, window
open Browser

// Import, jsNative
open Fable.Core
// ? operator
open Fable.Core.JsInterop

open Save_Load_Types

(* Helper functions - storage *)

[<Global("indexedDB")>]
let indexedDB: obj = jsNative

// TODO2 #save This might no longer be needed because we store saved games in indexeddb instead of local storage.
let get_usage () : JS.Promise<Save_Load_Usage_Data> =
    let bytes_to_megabytes (bytes: float) = bytes / 1024.0 / 1024.0

    promise {
(* To use this, run
dotnet add package Fable.Browser.Navigator
(end)
*)
        let! estimate = navigator?storage?estimate ()
        return {
            usage = estimate?usage |> unbox |> bytes_to_megabytes
            quota = estimate?quota |> unbox |> bytes_to_megabytes
        }
    }

let open_db
    (database_configuration : Database_Configuration) =
(* open is an F# keyword, so we must escape it. *)
    let request = indexedDB?``open`` (database_configuration.database_name, 1)

    request?onupgradeneeded <- (fun _ ->
        let db = request?result
        let store = db?createObjectStore (database_configuration.store_name, {| keyPath = "id"; autoIncrement = true |})
(* It seems it is necessary to add a record to establish the fields for this table. We believe this also establishes the initial value of the autoincremented id field. *)
        store?add {|
            id = highest_built_in_record_id
            name = String.Empty
            timestamp = DateTime.UtcNow
            screenshot = String.Empty
            runner_saveable_state_json = String.Empty 
        |}
        store?delete highest_built_in_record_id
    )

    request

let add_saved_game_to_storage_3 (store : obj) (saved_game : New_Saved_Game) : obj =
    store?add {|
        name = saved_game.name
        timestamp = saved_game.timestamp
        screenshot = saved_game.screenshot
        runner_saveable_state_json = saved_game.runner_saveable_state_json    
    |}

let overwrite_saved_game_in_storage_3 (store : obj) (saved_game : Existing_Saved_Game) : obj =
    store?put {|
        id = saved_game.id
        name = saved_game.name
        timestamp = saved_game.timestamp
        screenshot = saved_game.screenshot
        runner_saveable_state_json = saved_game.runner_saveable_state_json
    |}

let add_autosave_or_quicksave_to_storage_3
    (store : obj)
    (runner_saveable_state_json : string)
    (screenshot : string)
    (autosave_or_quicksave : Autosave_or_Quicksave)
    : obj =
(* For quicksave and autosave, we always overwrite existing records.
We can use put even if the record does not yet exist. *)
    store?put {|
        id = match autosave_or_quicksave with | Quicksave -> quicksave_record_id | Autosave -> autosave_record_id
        name = match autosave_or_quicksave with | Quicksave -> "_Quicksave" | Autosave -> "_Autosave"
        timestamp = DateTime.UtcNow
        screenshot = screenshot
        runner_saveable_state_json = runner_saveable_state_json
    |}
