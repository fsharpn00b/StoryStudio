module Save_Load_Types

// DateTime
open System

(* Types - public *)

type Saved_Game_Action =
    | Save_Game
    | Load_Game
    | Delete_Game

(* Types - formerly private *)

type Saved_Game = {
    name : string
    screenshot : string
    timestamp : DateTime
(* This is serialized/deserialized by Runner_State.load_game () and .show_save (). *)
    game_state : string
}

type Saved_Games = Map<string, Saved_Game>

type Save_Load_Usage_Data = {
    usage : float
    quota : float
}

type Save_Load_State = {
    action : Saved_Game_Action
    current_game_state : string
    screenshot : string
    usage : Save_Load_Usage_Data
    saved_games : Saved_Games
    is_visible : bool
}

type Save_Load_Show_Data = {
    action : Saved_Game_Action
    current_game_state : string
    screenshot : string
    usage : Save_Load_Usage_Data
}

type Save_Load_Message =
    | Set_Saved_Games_In_State of Saved_Games
    | Show of Save_Load_Show_Data
    | Hide
    | Switch of Saved_Game_Action
    | Message_Save of Saved_Game
    | Message_Load of string
    | Message_Delete of string

(* Interfaces *)

type I_Save_Load =
    abstract member show : Saved_Game_Action -> string -> unit
    abstract member hide : unit -> unit
    abstract member switch : Saved_Game_Action -> unit
    abstract member is_visible : unit -> bool
    abstract member export : unit -> unit
    abstract member import : unit -> unit
    abstract member download_screenshot : unit -> unit

(* Consts *)

let database_name = "vnf_saved_games"
let store_name = "vnf_saved_games"
let database_row_id = "singleton"
let date_time_format = "yyyyMMdd_HHmmss"
let screenshot_element_id = "root"

let initial_state = {
    action = Load_Game
    current_game_state = String.Empty
    screenshot = String.Empty
    usage = { usage = 0.0; quota = 0.0 }
    is_visible = false
    saved_games = Map.empty
}
