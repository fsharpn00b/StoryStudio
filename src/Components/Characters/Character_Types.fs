module Character_Types

// IRefValue
open Fable.React

open Transition_Types
open Units_Of_Measure

(* Types - public *)

type Characters_Configuration = {
    placeholder : unit
}

type Character_Sprites = Map<string, string>

type Character_Input = {
    full_name : string
    short_name : string
(* Height does not change during character animations. *)
    height : int<percent>
    sprites : Character_Sprites
}

type Character_Input_Map = Map<string, Character_Input>

(* TODO2 We initially thought the following.

Visible_Character_Data does not really need to be wrapped inside Fade_State. Fade_State really only needs the URL, so it knows whether to ignore a Fade_In or Cross_Fade message (if the old URL is the same as the new URL).
Character position and height are only needed here, in Characters, for the view function.

The height field is duplicated between this type and Visible_Character_Data. We need it here because Visible_Character_Data is ephemeral. It is re-created every time we want to show the character in a different position or with a different sprite (which means a different URL).

We could remove height and url from Visible_Character_Data, and only store height here and url in Fade_State. However, Visible_Character_Data is a convenient encapsulation. It simplifies several functions so they do not require the entire Character type. Having Visible_Character_Data wrapped inside Fade_State has not, so far, created any issues with handling the intersections of various state types.
(end)

However, we later realized that we update not only the fade state, but the position, by dispatching Visible_Character_Data. Dispatch not only sends the URL to the Fade component, it also mutates the character state, because we created state and dispatch for each character with React.useElmish. Having Visible_Character_Data contain both URL and position means we only have one dispatch call to make.

Ideally, we can decorate a character with various animations - fade, move, etc. The question is how to handle the intersections of different animation types. For example, if we apply a move animation, how does that affect the fade state? Maybe the move is only valid if the fade state is Idle_Visible, and so on.

See Dialogue_Box for how to use multiple state types (in its case, fade and typing).
*)
type Visible_Character_Data = {
    position : int<percent>
    height : int<percent>
    url : string
}

type Character_State =
    | Visible of Visible_Character_Data
    | Hidden

type Character_Move_In_Or_Out =
    | In
    | Out

type Character_Move_Direction =
    | Left
    | Right
    | Bottom

type Character_Move_Data_1 = {
    in_or_out : Character_Move_In_Or_Out
    direction : Character_Move_Direction
}

type Character_Transition_Type =
    | Fade
    | Move of Character_Move_Data_1

(* These types are used by the parser to create commands. *)

type Character_Fade_In_Data = {
(* This is for debugging. *)
    character_short_name : string
    url : string
    position : int<percent>
    transition_time : Transition_Time
}

type Character_Fade_Out_Data = {
    character_short_name : string
    transition_time : Transition_Time
}

type Character_Cross_Fade_Data = {
    character_short_name : string
    url : string
    transition_time : Transition_Time
}

type Character_Move_In_Data = {
    character_short_name : string
    url : string
    direction : Character_Move_Direction
    position : int<percent>
    transition_time : Transition_Time
}

type Character_Move_Out_Data = {
    character_short_name : string
    direction : Character_Move_Direction
    transition_time : Transition_Time
}

type Character_Move_Data_2 =
    | In of Character_Move_In_Data
    | Out of Character_Move_Out_Data

type Characters_Saveable_State = Map<string, Character_State>

(* Interfaces *)

type I_Character =
    abstract member get_id : unit -> int<character_id>
    abstract member get_full_name : unit -> string
(* url, position, transition_time, command_queue_item_id *)
    abstract member fade_in : string -> int<percent> -> Transition_Time -> int<runner_queue_item_id> -> unit
(* transition_time, command_queue_item_id *)
    abstract member fade_out : Transition_Time -> int<runner_queue_item_id> -> unit
(* url, transition_time, command_queue_item_id *)
    abstract member cross_fade : string -> Transition_Time -> int<runner_queue_item_id> -> unit
(* url, direction, position, transition_time, command_queue_item_id *)
    abstract member move_in : string -> Character_Move_Direction -> int<percent> -> Transition_Time -> int<runner_queue_item_id> -> unit
(* direction, transition_time, command_queue_item_id *)
    abstract member move_out : Character_Move_Direction -> Transition_Time -> int<runner_queue_item_id> -> unit
    abstract member get_state : unit -> Character_State
    abstract member set_state : Character_State -> unit
(* This is for debugging. *)
    abstract member get_character_data : unit -> {|
        id : int<character_id>
        short_name : string
        full_name : string
        state : Transition_State<Character_State, Character_Transition_Type>
        transition_timeout_function_handle : float option
    |}

type I_Characters =
(* character_short_name, url, position, transition_time, command_queue_item_id *)
    abstract member fade_in : string -> string -> int<percent> -> Transition_Time -> int<runner_queue_item_id> -> unit
(* character_short_name, transition_time, command_queue_item_id *)
    abstract member fade_out : string -> Transition_Time -> int<runner_queue_item_id> -> unit
(* transition_time, command_queue_item_id *)
    abstract member fade_out_all : Transition_Time -> int<runner_queue_item_id> -> unit
(* character_short_name, url, transition_time, command_queue_item_id *)
    abstract member cross_fade : string -> string -> Transition_Time -> int<runner_queue_item_id> -> unit
(* character_short_name, url, direction, position, transition_time, command_queue_item_id *)
    abstract member move_in : string -> string -> Character_Move_Direction -> int<percent> -> Transition_Time -> int<runner_queue_item_id> -> unit
(* character_short_name, direction, transition_time, command_queue_item_id *)
    abstract member move_out : string -> Character_Move_Direction -> Transition_Time -> int<runner_queue_item_id> -> unit
    abstract member get_state : unit -> Characters_Saveable_State
    abstract member set_state : Characters_Saveable_State -> unit
    abstract member set_configuration : Characters_Configuration -> unit
    abstract member get_configuration : unit -> Characters_Configuration
    abstract member character_short_name_to_full_name : string -> string
(* This is for debugging. *)
    abstract member get_character_data : unit -> unit

(* This must be defined after I_Character. *)
(* Character_Map is just a container. We do not need to change it because we do not add or remove characters after we initialize it. Nor do we need to replace the characters within Character_Map to change them, because character states are mutable, not immutable. *)
type Character_Map = Map<string, IRefValue<I_Character>>
