module Save_Load_Storage

// DateTime
open System

// document, navigator, FileReader, window
open Browser
// Import, jsNative
open Fable.Core
// ? operator
open Fable.Core.JsInterop
// IRefValue
open Feliz
// Decode, Encode
open Thoth.Json

open Log
open Save_Load_Storage_Helpers
open Save_Load_Types
open Units_Of_Measure

(* See:
https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API/Using_IndexedDB
(end)
*)

(* Debug *)

let debug_module_name = "Save_Load_Storage"

let private debug : log_function = debug debug_module_name
let private warn
    (function_name : string)
    (alert : bool)
    (message : string)
    (data : (string * obj) list)
    : unit =
    warn debug_module_name function_name alert $"{message} {warn_recommendation}" data
let private error : error_function = error debug_module_name

(* Main functions - get saved games *)

let get_saved_game_display_data_from_storage ()
    : JS.Promise<Saved_Games_Display_Data> =

    Promise.create (fun resolve reject ->
        let request_1 = open_db ()
        let mutable results_1 = Map.empty

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (store_name, "readonly")
            let store = tx?objectStore store_name

            let request_2 = store?openCursor ()
            request_2?onsuccess <- (fun event ->
                let cursor = event?target?result
                match cursor with
                | None -> resolve results_1
                | Some results_2 ->
                    do
                        results_1 <- results_1.Add (results_2?key |> LanguagePrimitives.Int32WithMeasure, { name = results_2?value?name; screenshot = results_2?value?screenshot })
(* continue is a reserved word, so we must enclose it with backticks. *)
                        cursor?``continue`` ()
            )

            request_2?onerror <- (fun ex ->
                warn "get_saved_game_display_data_from_storage" true "Failed to read saved games." ["error", ex]
                reject <| new Exception "Failed to read saved games."
            )
        )

        request_1?onerror <- (fun ex ->
            warn "get_saved_game_display_data_from_storage" true "Failed to open database." ["error", ex]
            reject <| new Exception "Failed to open database."
        )
    )

let get_saved_game_from_storage
    (saved_game_id : int<saved_game_id>)
    : JS.Promise<string> =

(* This Promise is really just adding another layer with the same intent as onsuccess/onerror. However, it seems it is more idiomatic to return a Promise up the call stack until we reach a function that has a dispatch () function, such as view () or one of its helpers, than to pass the dispatch () function down the call stack.
The demarcation between (1) the action we perform in the database and (2) its result can be represented by onsuccess/onerror, Promise, or dispatch ().
*)
    Promise.create (fun resolve reject ->
        let request_1 = open_db ()

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (store_name, "readonly")
            let store = tx?objectStore store_name

            let request_2 = store?get saved_game_id
            request_2?onsuccess <- (fun _ ->
                match request_2?result with
                | None ->
                    do
                        warn "get_saved_game_from_storage" true "Failed to get saved game." ["saved_game_id", saved_game_id]
                        reject <| new Exception "Failed to get saved game."
                | Some result_2 -> do resolve result_2?game_state
            )

            request_2?onerror <- (fun ex ->
                do
                    warn "get_saved_game_from_storage" true "Failed to get saved game." ["saved_game_id", saved_game_id; "error", ex]
                    reject <| new Exception "Failed to get saved game."
            )
        )

        request_1?onerror <- (fun ex ->
            do
                warn "get_saved_game_from_storage" true "Failed to open database." ["error", ex]
                reject <| new Exception "Failed to open database."
        )
    )

let get_all_saved_games_from_storage ()
    : JS.Promise<Existing_Saved_Game list> =

    Promise.create (fun resolve reject ->
        let request_1 = open_db ()
        let mutable results_1 = []

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (store_name, "readonly")
            let store = tx?objectStore store_name

            let request_2 = store?openCursor ()
            request_2?onsuccess <- (fun event ->
                let cursor = event?target?result
                match cursor with
                | None -> resolve results_1
                | Some results_2 ->
                    let saved_game = {
                        id = results_2?key
                        name = results_2?value?name
                        timestamp = results_2?value?timestamp
                        screenshot = results_2?value?screenshot
                        game_state = results_2?value?game_state 
                    }
                    do
                        results_1 <- saved_game :: results_1
                        cursor?``continue`` ()
            )

            request_2?onerror <- (fun ex ->
                warn "get_all_saved_games_from_storage" true "Failed to read saved games." ["error", ex]
                reject <| new Exception "Failed to read saved games."
            )
        )

        request_1?onerror <- (fun ex ->
            warn "get_all_saved_games_from_storage" true "Failed to open database." ["error", ex]
            reject <| new Exception "Failed to open database."
        )
    )

(* Main functions - add saved games *)

let add_saved_game_to_storage_1
    (saved_game : New_Saved_Game)
    : unit =

    let request_1 = open_db ()

    request_1?onsuccess <- (fun _ ->
        let db = request_1?result
        let tx = db?transaction (store_name, "readwrite")
        let store = tx?objectStore store_name
        let request_2 = add_saved_game_to_storage_2 store saved_game
        request_2?onerror <- (fun ex -> do warn "add_saved_game_to_storage" true "Failed to store saved game." ["error", ex])
    )

    request_1?onerror <- (fun ex -> do warn "add_saved_game_to_storage" true "Failed to open database." ["error", ex])

let add_saved_games_to_storage (saved_games : New_Saved_Game list) : unit =
    let request_1 = open_db ()

    request_1?onsuccess <- (fun _ ->
        let db = request_1?result
        let tx = db?transaction (store_name, "readwrite")
        let store = tx?objectStore store_name

        do saved_games |> List.iter (fun saved_game ->
            let request_2 = add_saved_game_to_storage_2 store saved_game
            request_2?onerror <- (fun ex -> do warn "set_saved_games_in_storage" true "Failed to store saved games." ["error", ex])
        )
    )

    request_1?onerror <- (fun ex -> do warn "set_saved_games_in_storage" true "Failed to open database." ["error", ex])

let add_quicksave_or_autosave_to_storage_1
    (current_game_state : string)
    (quicksave_or_autosave : Quicksave_Or_Autosave)
    : unit =

    promise {
        let! canvas = get_canvas ()
        return canvas
    } |> Promise.iter (fun canvas ->
        let screenshot = downscale_screenshot canvas screenshot_max_width screenshot_mime_type screenshot_encoder_options
        let request_1 = open_db ()

        request_1?onsuccess <- (fun _ ->
            let db = request_1?result
            let tx = db?transaction (store_name, "readwrite")
            let store = tx?objectStore store_name

            let request_2 = add_quicksave_or_autosave_to_storage_2 store current_game_state screenshot quicksave_or_autosave
            request_2?onerror <- (fun ex -> do warn "add_quicksave_or_autosave_to_storage_1" true "Failed to store saved game." ["error", ex])
        )

        request_1?onerror <- (fun ex -> do warn "add_quicksave_or_autosave_to_storage_1" true "Failed to open database." ["error", ex])
    )

(* Main functions - update saved games *)

let overwrite_saved_game_in_storage_1
    (saved_game : Existing_Saved_Game)
    : unit =

    let request_1 = open_db ()

    request_1?onsuccess <- (fun _ ->
        let db = request_1?result
        let tx = db?transaction (store_name, "readonly")
        let store = tx?objectStore store_name

        let request_3 = overwrite_saved_game_in_storage_2 store saved_game
        request_3?onerror <- (fun ex -> do warn "overwrite_saved_game_in_storage" true "Failed to store saved game." ["saved_game_id", saved_game.id; "error", ex])
    )

    request_1?onerror <- (fun ex -> warn "overwrite_saved_game_in_storage" true "Failed to open database." ["error", ex])

(* Main functions - delete saved games *)

let delete_saved_game_from_storage
    (saved_game_id : int<saved_game_id>)
    : unit =

    let request_1 = open_db ()

    request_1?onsuccess <- (fun _ ->
        let db = request_1?result
        let tx = db?transaction (store_name, "readwrite")
        let store = tx?objectStore store_name
        let request_2 = store?delete saved_game_id

        request_2?onerror <- (fun ex -> do warn "delete_saved_game_from_storage" true "Failed to delete saved game." ["saved_game_id", saved_game_id; "error", ex])
    )

    request_1?onerror <- (fun ex -> do warn "delete_saved_game_from_storage" true "Failed to open database." ["error", ex])

let delete_all_saved_games_from_storage () : unit =
    let request_1 = open_db ()

    request_1?onsuccess <- (fun _ ->
        let db = request_1?result
        let tx = db?transaction (store_name, "readwrite")
        let store = tx?objectStore store_name
        let request_2 = store?clear

        request_2?onerror <- (fun ex -> do warn "delete_all_saved_games_from_storage" true "Failed to delete saved game." ["error", ex])
    )

    request_1?onerror <- (fun ex -> do warn "delete_all_saved_games_from_storage" true "Failed to open database." ["error", ex])

(* Main functions - export saved games *)

let export_saved_games_from_storage_to_file () : unit =
    let file_name = $"{get_current_timestamp ()}.json"
    get_all_saved_games_from_storage ()
        |> Promise.iter (fun saved_games_1 ->
(* Disard the database record IDs before exporting. If we re-import these saved games, we will assign new database record IDs then. *)
            let saved_games_2 = saved_games_1 |> List.map (fun saved_game ->
                {
                    name = saved_game.name
                    screenshot = saved_game.screenshot
                    timestamp = saved_game.timestamp
                    game_state = saved_game.game_state
                }
            )
            let json = Encode.Auto.toString (0, saved_games_2)
            download_file file_name "text/json" json
        )

let export_current_game_to_file (current_game_state : string) : unit =
    match window.prompt ("Enter save name:", get_current_timestamp ()) with
    | null -> ()
    | save_name ->
        promise {
            let! canvas = get_canvas ()
            return canvas
        } |> Promise.iter (fun canvas ->
            let file_name = $"{get_current_timestamp ()}.json"
            let saved_game = {
                name = save_name
                screenshot = downscale_screenshot canvas screenshot_max_width screenshot_mime_type screenshot_encoder_options
                timestamp = DateTime.UtcNow
                game_state = current_game_state
            }
(* import_saved_games_from_file () expects a list of saved games. *)
            let json = Encode.Auto.toString (0, [saved_game])
            download_file file_name "text/json" json
        )

(* Main functions - import saved games *)

let import_saved_games_from_file_to_storage () : unit =

    let handle_file (file_name : string) (file_contents : string) : unit =
        match Decode.Auto.fromString<New_Saved_Game list> file_contents with
        | Ok saved_games -> do add_saved_games_to_storage saved_games
        | Error error ->
            do warn "import_saved_games_from_file_to_storage" true "Failed to deserialize saved games from file." ["file_name", file_name; "error", error]

    open_read_file_dialog handle_file

(* This lets players save and load their game even if they do not have indexeddb support. *)
// TODO1 However, if no indexeddb support, they'll need a way to disable autosave.
let import_current_game_from_file
    (dispatch : Save_Load_Message -> unit)
    : unit =

    let handle_file (file_name : string) (file_contents : string) : unit =
        match Decode.Auto.fromString<New_Saved_Game list> file_contents with
        | Ok (head :: _) ->
            if window.confirm $"Load save '{head.name}'? Current progress will be lost." then
                do dispatch <| Message_Load_Game head.game_state
        | Ok [] -> do warn "import_current_game_from_file" true "File does not contain any saved games." ["file_name", file_name]
        | Error error ->
            do warn "import_current_game_from_file" true "Failed to deserialize saved games from file." ["file_name", file_name; "error", error]

    open_read_file_dialog handle_file
