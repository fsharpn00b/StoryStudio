module Save_Load_Helpers

// DateTime
open System

// Blob, console, document, FileReader, window
open Browser
// Element, HTMLAnchorElement, HTMLCanvasElement
open Browser.Types
// Import, jsNative
open Fable.Core
// ? operator
open Fable.Core.JsInterop

open Log
open Save_Load_Types

(* Debug *)

let debug_module_name = "Save_Load_Helpers"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

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

[<ImportDefault("html2canvas")>]
let private html_to_canvas_with_options (element : Element) (options : obj) : JS.Promise<HTMLCanvasElement> = jsNative

let get_canvas (ignore_save_load_and_configuration : bool) : JS.Promise<Result<HTMLCanvasElement, string * Error_Data>> =
(* To use this, run
dotnet add package Fable.Promise
(end)
*)
    promise {
        try
            let element = document.getElementById screenshot_element_id
            let! canvas =
                if ignore_save_load_and_configuration then
(* We can also tell html2canvas to ignore an element by adding the following property to the element:
prop.custom ("data-html2canvas-ignore", "true")
(end)
*)
                    let ignored_element_ids = set [ "save_load_screen"; "configuration_screen" ]
                    let ignore_elements (element : Browser.Types.Element) : bool =
                        ignored_element_ids.Contains element.id
                    let options =
                        createObj [
(* This field must take a function that returns a bool. *)
                            "ignoreElements" ==> System.Func<Browser.Types.Element, bool>(ignore_elements)
                        ]
                    html_to_canvas_with_options element options
                else
                    html_to_canvas element
            return Ok canvas

        with e ->
            return Error ("Failed to get canvas.", ["error", e])
    }

(* Previously, this failed if the background was hidden. This is probably because the background determines the canvas size. See the comment in background.css/.background_fade_image. We now handle this case by filling the canvas with black. *)
let downscale_screenshot
    (canvas: HTMLCanvasElement)
    (max_width: int)
    (mime_type : string)
    (encoder_options : float)
    : Result<string, string * Error_Data> =

    try
        let offscreen = document.createElement "canvas" :?> HTMLCanvasElement
        let ctx = offscreen.getContext_2d ()

        if max_width <= 0 then
            Error ("Invalid screenshot width.", ["max_width", max_width])
        elif canvas.width <= 0 || canvas.height <= 0 then
(* Work around html2canvas returning a zero-size canvas when one or more key UI elements are hidden. *)
            offscreen.width <- max_width
            offscreen.height <- max_width
            ctx.fillStyle <- U3.Case1 "black"
            ctx.fillRect (0., 0., float offscreen.width, float offscreen.height)
            Ok <| offscreen.toDataURL (mime_type, encoder_options)
        else
            let scale = float max_width / float canvas.width
            let new_width = max_width
            let new_height = max 1 (int (float canvas.height * scale))
            offscreen.width <- new_width
            offscreen.height <- new_height
            ctx.drawImage (U3.Case2 canvas, 0., 0., float new_width, float new_height)
            Ok <| offscreen.toDataURL (mime_type, encoder_options)

    with e ->
        Error ("Failed to downscale screenshot.", ["error", e])

let private download_screenshot_2 
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

(* This is used by I_Save_Load.download_screenshot (). *)
let download_screenshot_1 () : unit =
    promise {
(* For manual screenshots, do not exclude the save/load or configuration screens, as the player might be trying to report a bug in them. *)
        let! result = get_canvas false
        match result with

        | Ok canvas ->
            do download_screenshot_2 canvas "image/png"
        
        | Error (message, error_data) ->
            warn "download_screenshot_1" true message error_data
    } |> Promise.iter ignore
