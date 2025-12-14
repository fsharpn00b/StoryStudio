module Save_Load_Storage

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
open Save_Load_Types
open Utilities

(* Debug *)

let debug_module_name = "Save_Load_Storage"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Helper functions - screenshots *)

(* To use this, run
npm install html2canvas
(end)
*)
[<ImportDefault("html2canvas")>]
let private html_to_canvas (element : Element) : JS.Promise<HTMLCanvasElement> = jsNative

let get_canvas () : JS.Promise<HTMLCanvasElement> =
(* To use this, run
dotnet add package Fable.Promise
(end)
*)
    promise {
        let element = document.getElementById screenshot_element_id
        let! canvas = html_to_canvas element
        return canvas
    }

let inline download_file
    (file_name : string)
    (mime_type : string)
    (blob : 'a) : unit =
(* type is an F# keyword, so we must escape it. *)
    let blob = Blob.Create ([| blob |], jsOptions (fun o -> o?``type`` <- mime_type))

    let url: string = emitJsExpr blob "URL.createObjectURL($0)"

(* Create a hidden <a> element. *)
    let a = document.createElement "a" :?> HTMLAnchorElement
    a?href <- url
    a?download <- file_name
    a?style?display <- "none"

    document.body.appendChild a |> ignore
    a.click()
    document.body.removeChild a |> ignore

    emitJsExpr url "URL.revokeObjectURL($0)"

let downscale_screenshot
    (canvas: HTMLCanvasElement)
    (max_width: int)
    (mime_type : string)
    (encoder_options : float)
    : string =

    let scale = float max_width / float canvas.width
    let new_width = max_width
    let new_height = int (float canvas.height * scale)

    let offscreen = document.createElement "canvas" :?> HTMLCanvasElement
    offscreen.width <- new_width
    offscreen.height <- new_height

    let ctx = offscreen.getContext_2d ()
    ctx.drawImage (U3.Case2 canvas, 0., 0., float new_width, float new_height)

    offscreen.toDataURL (mime_type, encoder_options)

let download_screenshot_2 
    (canvas: HTMLCanvasElement)
    (mime_type : string)
    : unit =

    let offscreen = document.createElement "canvas" :?> HTMLCanvasElement
    offscreen.width <- canvas.width
    offscreen.height <- canvas.height

    let ctx = offscreen.getContext_2d ()
    ctx.drawImage (U3.Case2 canvas, 0., 0., float canvas.width, float canvas.height)

    offscreen.toBlob ((fun blob ->
        let file_name = $"{get_current_timestamp ()}.png"
        download_file file_name mime_type blob
    ), "image/png")

(* Helper functions - storage *)

(* TODO1 Save games as database rows instead of a single json string. First we need to find out how widely supported indexed DB is. If it's not supported, might need to fall back to local storage. Supported across major browsers. Does not seem to work on surf.
*)

[<Global("indexedDB")>]
let indexedDB: obj = jsNative

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

let private open_db () =
(* open is an F# keyword, so we must escape it. *)
    let request = indexedDB?``open`` (database_name, 1)

    request?onupgradeneeded <- fun _ ->
        let db = request?result
(* It seems we are required to have a key column. See also set_saved_games (). *)
        db?createObjectStore (store_name, {| keyPath = "id" |}) |> ignore

    request

(* Main functions - state, storage *)

let get_saved_games_from_storage (dispatch : Save_Load_Message -> unit) : unit =
    let request_1 = open_db ()

    request_1?onsuccess <- fun _ ->
        let db = request_1?result
        let tx = db?transaction (store_name, "readonly")
        let store = tx?objectStore store_name
        let request_2 = store?get database_row_id

        request_2?onsuccess <- fun _ ->
            match request_2?result with
(* The initial state contains no saved games, so leave it unchanged. *)
            | None -> ()
            | Some result_3 ->
(* We need to get the contents of the json column. *)
                match Decode.Auto.fromString<Saved_Games> result_3?json with
                | Ok saved_games -> do dispatch <| Set_Saved_Games_In_State saved_games 
                | Error error_ ->
                    error "get_saved_games" "Failed to deserialize saved games." ["data", result_3; "error", error_] |> invalidOp

        request_2?onerror <- fun ex -> error "get_saved_games" "Failed to read saved games." ["error", ex]

    request_1?onerror <- fun ex -> error "get_saved_games" "Failed to open database." ["error", ex]

let set_saved_games_in_storage (saved_games : Saved_Games) : unit =
    let request_1 = open_db ()

    request_1?onsuccess <- fun _ ->
        let db = request_1?result
        let tx = db?transaction (store_name, "readwrite")
        let store = tx?objectStore store_name
        let json = Encode.Auto.toString (0, saved_games)
(* It seems we are required to have a key column. See also open_db (). *)
        let request_2 = store?put {| id = database_row_id; json = json |}

(* We do not want to crash while the player is trying to save. *)
        request_2?onerror <- fun ex ->
            do warn "set_saved_games" true "Failed to store saved games. Recommending exporting saved games to file." ["error", ex]

    request_1?onerror <- fun ex ->
        do warn "set_saved_games" true "Failed to open database. Recommending exporting saved games to file." ["error", ex]

let export_saved_games_to_file (saved_games : Saved_Games) : unit =
    let file_name = $"{get_current_timestamp ()}.json"
    let json = Encode.Auto.toString (0, saved_games)
    download_file file_name "text/json" json

let import_saved_games_from_file (dispatch : Save_Load_Message -> unit) : unit =

    let read_file (file: File) =
        let reader = FileReader.Create()
        reader.onload <- fun _ ->
            let json: string = reader.result |> unbox
            if window.confirm "Overwrite all saved games with imported backup file?" then
                match Decode.Auto.fromString<Saved_Games> json with
                | Ok saved_games ->
                    do
(* Update state. *)
                        dispatch <| Set_Saved_Games_In_State saved_games 
(* Update storage. *)
                        set_saved_games_in_storage saved_games
                | Error error ->
                    do warn "import_saved_games" true "Failed to deserialize saved games." ["error", error]

        reader.readAsText file

    let open_file_dialog () =
(* Create a hidden <input type="file">. *)
        let input = document.createElement "input" :?> HTMLInputElement
(* type is an F# keyword, so we must escape it. *)
        input?``type`` <- "file"
        input?style?display <- "none"

        input.onchange <- fun _ ->
            if input.files.length > 0 then
                let file = input.files.[0]
                read_file file

        document.body.appendChild input  |> ignore
        input.click()
        document.body.removeChild input |> ignore

    open_file_dialog ()
