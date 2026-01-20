module Save_Load_Storage_Helpers

// DateTime
open System

// Blob, navigator (also provides console, document, FileReader, window)
open Browser
// Element, HTMLAnchorElement, HTMLCanvasElement
open Browser.Types

// Import, jsNative
open Fable.Core
// ? operator
open Fable.Core.JsInterop

open Save_Load_Types

(* Helper functions - miscellaneous, public *)

let get_current_timestamp () : string =
    DateTime.UtcNow.ToString date_time_format

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

let open_read_file_dialog (handle_file : string -> string -> unit) =
(* Create a hidden <input type="file">. *)
    let input = document.createElement "input" :?> HTMLInputElement
(* type is an F# keyword, so we must escape it. *)
    input?``type`` <- "file"
    input?style?display <- "none"

    input.onchange <- (fun _ ->
        if input.files.length > 0 then
            let file = input.files.[0]
            let reader = FileReader.Create()
            reader.onload <- (fun _ -> do handle_file file.name (reader.result |> unbox))
            reader.readAsText file
    )

    document.body.appendChild input  |> ignore
    input.click()
    document.body.removeChild input |> ignore

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

let open_db () =
(* open is an F# keyword, so we must escape it. *)
    let request = indexedDB?``open`` (database_name, 1)

    request?onupgradeneeded <- (fun _ ->
        let db = request?result
        let store = db?createObjectStore (store_name, {| keyPath = "id"; autoIncrement = true |})
        store?add {|
            id = highest_built_in_record_id
            name = String.Empty
            timestamp = DateTime.UtcNow
            screenshot = String.Empty
            game_state = String.Empty 
        |}
        store?delete highest_built_in_record_id
    )

    request

let add_saved_game_to_storage_2 (store : obj) (saved_game : New_Saved_Game) : obj =
    store?add {|
        name = saved_game.name
        timestamp = saved_game.timestamp
        screenshot = saved_game.screenshot
        game_state = saved_game.game_state    
    |}

let overwrite_saved_game_in_storage_2 (store : obj) (saved_game : Existing_Saved_Game) : obj =
    store?put {|
        id = saved_game.id
        name = saved_game.name
        timestamp = saved_game.timestamp
        screenshot = saved_game.screenshot
        game_state = saved_game.game_state
    |}

let add_quicksave_or_autosave_to_storage_2
    (store : obj)
    (current_game_state : string)
    (screenshot : string)
    (quicksave_or_autosave : Quicksave_Or_Autosave)
    : obj =
(* We can use put even if the record does not yet exist. *)
    store?put {|
        id = match quicksave_or_autosave with | Quicksave -> quicksave_record_id | Autosave -> autosave_record_id
        name = match quicksave_or_autosave with | Quicksave -> "_Quicksave" | Autosave -> "_Autosave"
        timestamp = DateTime.UtcNow
        screenshot = screenshot
        game_state = current_game_state
    |}
