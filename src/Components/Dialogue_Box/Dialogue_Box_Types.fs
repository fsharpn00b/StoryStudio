module Dialogue_Box_Types

open Units_Of_Measure

(* Types - public *)

type Dialogue_Box_Visibility_State =
    | Visible
    | Hidden

type Dialogue_Box_Transition_Type = Fade

(* TODO2 Make these configurable.
/ Typing speed
    x Set to 0 to disable typing. If typing disabled, just tell typewriter to show entire string.
- These could be done via CSS. Or they could be a map we apply to the HTML in Dialogue_Box_Rendering.view ().
    - Font
    - Background color
    - Transparency
*)
type Dialogue_Box_Configuration = {
    typing_speed : Dialogue_Box_Typing_Speed
}

type Dialogue = {
    character_full_name : string
    text : string
}

type Dialogue_Box_Saveable_State = {
(* None means the typing state is Empty; Some means it is Idle. *)
    dialogue : Dialogue option
    visible : Dialogue_Box_Visibility_State
}

(* Interfaces *)

type I_Dialogue_Box =
    abstract member show : bool -> int<runner_queue_item_id> option -> unit
    abstract member hide : bool -> int<runner_queue_item_id> option -> unit
    abstract member is_visible : unit -> bool
    abstract member get_configuration : unit -> Dialogue_Box_Configuration
    abstract member set_configuration : Dialogue_Box_Configuration -> unit
    abstract member type_dialogue : string -> string -> int<runner_queue_item_id> -> unit
    abstract member get_state : unit -> Dialogue_Box_Saveable_State
    abstract member set_state : Dialogue_Box_Saveable_State -> unit

(* Types - formerly private *)

type Typing_State_Data = {
    character : string
    text : string
    typing_speed : Dialogue_Box_Typing_Speed
    index : int
    visible_text : string
    command_queue_item_id : int<runner_queue_item_id>
}

type Typing_State =
    | Empty
    | Idle of Dialogue
    | Typing of Typing_State_Data

type Begin_Typing_Message_Data = {
    character : string
    text : string
    typing_speed : Dialogue_Box_Typing_Speed
    command_queue_item_id : int<runner_queue_item_id>
}

type Typing_Message =
    | Begin_Typing of Begin_Typing_Message_Data
    | Reveal_Next of int<runner_queue_item_id>
    | Force_Complete_Typing
(* Set_Dialogue and Set_Empty do not dispatch Notify_Transition_Complete. See notes in Dialogue_Box_Typing.update_typing_state. *)
    | Set_Dialogue of Dialogue
    | Set_Empty
    | Notify_Transition_Complete of int<runner_queue_item_id>
