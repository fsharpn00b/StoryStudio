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

type Import_Saved_Games_File_Action =
    | Import_Current_Game
    | Import_All_Saved_Games

(* Types - formerly private *)

type Saved_Game_Display_Data = {
    name : string
    screenshot : string
}

// TODO2 #save Not sure why this is a map instead of just a list.
type Saved_Games_Display_Data = Map<int<saved_game_id>, Saved_Game_Display_Data>

(* This represents a saved game in indexeddb for display in the save/load screen. We do not export or import this to or from a file. *)
type Existing_Saved_Game = {
    id : int<saved_game_id>
    name : string
    screenshot : string
    timestamp : DateTime
(* This field (here or in New_Saved_Game) is deserialized by:
Save_Load_Validation.validate_and_parse_runner_saveable_state ()

It is serialized by:
Runner_Save_Load.autosave_or_quicksave ()
.export_current_game_to_file ()
.show_saved_game_screen ()
*)
    runner_saveable_state_json : string
}

(* This represents a saved game outside of indexeddb, such as the current game, or a game exported to a file. That is why we keep it separate from Existing_Saved_Game. *)
type New_Saved_Game = {
    name : string
    screenshot : string
    timestamp : DateTime
    runner_saveable_state_json : string
}

type Save_Load_Usage_Data = {
    usage : float
    quota : float
}

type Save_Load_Show_Data = {
    action : Saved_Game_Action
    runner_saveable_state_json : string
    saved_games : Saved_Games_Display_Data
    usage : Save_Load_Usage_Data
}

type Save_Load_State =
    | Hidden
    | Visible of Save_Load_Show_Data

type Save_Load_Message =
    | Redraw
    | Show of Save_Load_Show_Data
    | Hide
    | Switch of Saved_Game_Action

type Autosave_or_Quicksave = Quicksave | Autosave

(* Interfaces *)

type I_Save_Load =
    abstract member show : Saved_Game_Action -> string -> unit
    abstract member hide : unit -> unit
    abstract member switch : Saved_Game_Action -> unit
    abstract member is_visible : unit -> bool
    abstract member autosave_or_quicksave : string -> Autosave_or_Quicksave -> unit
(* These let players save and load their game even if they do not have indexeddb support. *)
    abstract member export_current_game_to_file : string -> unit
    abstract member import_current_game_from_file : unit -> unit
    abstract member download_screenshot : unit -> unit

(* Consts *)

let database_name = "story_studio"
(* TODO1 #save This will cause conflicts if there are multiple Story Studio games in the same browser. We should instead read this from a configuration file. *)
let store_name = "story_studio_saved_games"
(* When the user exports either the current game or all saved games to a file, we include the records IDs. However, when we import one or more saved games from a file, we assign new record IDs. We only use record IDs for quicksave and autosave.

TODO2 #save If the player creates a new saved game and enters the name of an existing saved game, we overwrite the existing saved game. This is probably not needed, as a new saved game with a previously used name would still have a different ID.

TODO2 #save When we import a saved game, we discard the ID unless it is autosave or quicksave. Otherwise we just add the saved game to storage and get a new ID. It is possible to introduce a name collision this way, but the IDs are still different.

TODO2 #save Do not let the player enter "quicksave" or "autosave" as saved game names? Again, this is not strictly needed, but might prevent confusion.
 *)
let non_autosave_or_quicksave_record_id = 1<saved_game_id>
let autosave_record_id = non_autosave_or_quicksave_record_id + 1<saved_game_id>
let quicksave_record_id = autosave_record_id + 1<saved_game_id>
let highest_built_in_record_id = quicksave_record_id

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
