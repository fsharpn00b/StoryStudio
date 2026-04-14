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

[<ImportDefault("html2canvas")>]
let private html_to_canvas_with_options (element : Element) (options : obj) : JS.Promise<HTMLCanvasElement> = jsNative

let get_canvas (ignore_save_load_and_configuration : bool) : JS.Promise<HTMLCanvasElement> =
(* To use this, run
dotnet add package Fable.Promise
(end)
*)
    promise {
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
        let! canvas = get_canvas false
        return canvas
    } |> Promise.iter (fun canvas ->
        download_screenshot_2 canvas "image/png"
    )
