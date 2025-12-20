module Units_Of_Measure

(* Note Units of measure do not work with unsigned ints. *)

[<Measure>] type milliseconds
[<Measure>] type percent
[<Measure>] type seconds

[<Measure>] type scene_id
[<Measure>] type command_id
[<Measure>] type command_queue_item_id
[<Measure>] type command_queue_order
[<Measure>] type character_id

[<Measure>] type saved_game_id

type Fade_Transition_Time = float<seconds>
type Dialogue_Box_Typing_Speed = int<milliseconds>
