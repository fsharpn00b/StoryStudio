module Save_Load_Types

// DateTime
open System

// console, window
open Browser.Dom
// ? operator
open Fable.Core.JsInterop

open Units_Of_Measure

(* Types - public *)

type Saved_Game_Action =
    | Save_Game
    | Load_Game
    | Delete_Game

(* Types - formerly private *)

type Saved_Game_Display_Data = {
    name : string
    screenshot : string
}

type Saved_Games_Display_Data = Map<int<saved_game_id>, Saved_Game_Display_Data>

type Existing_Saved_Game = {
    id : int<saved_game_id>
    name : string
    screenshot : string
    timestamp : DateTime
(* This is serialized/deserialized by Runner_State.load_game () and .show_save (). *)
    game_state : string
}

type New_Saved_Game = {
    name : string
    screenshot : string
    timestamp : DateTime
    game_state : string
}

type Save_Load_Usage_Data = {
    usage : float
    quota : float
}

type Save_Load_Show_Data = {
    action : Saved_Game_Action
    current_game_state : string
    screenshot : string
    saved_games : Saved_Games_Display_Data
    usage : Save_Load_Usage_Data
}

type Save_Load_State =
    | Hidden
    | Visible of Save_Load_Show_Data

type Save_Load_Message =
    | Show of Save_Load_Show_Data
    | Hide
    | Switch of Saved_Game_Action
    | Message_Save_New_Game of New_Saved_Game
    | Message_Save_Existing_Game of Existing_Saved_Game
    | Message_Load_Game of string
    | Message_Delete_Game of int<saved_game_id>
    | Message_Delete_All_Games

type Quicksave_Or_Autosave = Quicksave | Autosave

(* Interfaces *)

type I_Save_Load =
    abstract member show : Saved_Game_Action -> string -> unit
    abstract member hide : unit -> unit
    abstract member switch : Saved_Game_Action -> unit
    abstract member is_visible : unit -> bool
    abstract member quicksave_or_autosave : string -> Quicksave_Or_Autosave -> unit
    abstract member export_saved_games_from_storage_to_file : unit -> unit
    abstract member import_saved_games_from_file_to_storage : unit -> unit
    abstract member export_current_game_to_file : string -> unit
    abstract member import_current_game_from_file : unit -> unit
    abstract member download_screenshot : unit -> unit

(* Consts *)

let database_name = "vnf_saved_games"
let store_name = "vnf_saved_games"
let quicksave_record_id = 1<saved_game_id>
let autosave_record_id = quicksave_record_id + 1<saved_game_id>
let highest_built_in_record_id = autosave_record_id

let date_time_format = "yyyyMMdd_HHmmss"

let screenshot_element_id = "root"
let screenshot_max_width = 160
let screenshot_mime_type = "image/jpeg"
let screenshot_encoder_options = 0.7

[<Literal>]
let export_current_game_key = "x"
let warn_recommendation = $"To avoid data loss, recommend exporting current game by pressing '{export_current_game_key}' key."
let is_indexeddb_supported =
    let inline exists (name : string) = not (isNull (window?(name)))
    exists "indexedDB"
    || exists "mozIndexedDB"
    || exists "webkitIndexedDB"
    || exists "msIndexedDB"

let initial_state = Hidden
