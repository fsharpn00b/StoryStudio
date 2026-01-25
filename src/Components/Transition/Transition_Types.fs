module Transition_Types

open Units_Of_Measure

(* Types - public *)

type Transition_Configuration = unit

(* Previously, when this was Fade_Transition, 'T was typically a string that contained a background URL or character sprite URL. Now, 'T represents a higher type such as Visible of string (URL) | Hidden. So we have moved the transition type (for example, fade in/fade out/cross fade) and corresponding state data (visible (url)/hidden) from here to the component (such as Background). That is why Transition_Data contains both old_data and new_data fields, which would not have applied to, for example, fade in (which only needs a new URL) or fade out (which only needs an old URL).
*)
type Transition_Data<'data_type, 'transition_type> = {
    old_data : 'data_type
    new_data : 'data_type
    transition_type : 'transition_type
    transition_time : Transition_Time
    command_queue_item_id : int<runner_queue_item_id>
}

type Transition_State<'data_type, 'transition_type> =
    | Idle of 'data_type
    | Pre_Transition of Transition_Data<'data_type, 'transition_type>
    | In_Transition of Transition_Data<'data_type, 'transition_type>

type Transition_Message_Data<'data_type, 'transition_type> = {
    new_data : 'data_type
    transition_type : 'transition_type
    transition_time : Transition_Time
    command_queue_item_id : int<runner_queue_item_id>
}

type Skip_Transition_Message_Data<'data_type, 'transition_type> = {
    new_data : 'data_type
    transition_type : 'transition_type
    is_notify_transition_complete : bool
    command_queue_item_id : int<runner_queue_item_id> option
}

(* Transition and Skip_Transition are sent by Runner. Begin_Transition and Complete_Transition are sent by timeout functions. *)
type Transition_Message<'data_type, 'transition_type> =
    | Transition of Transition_Message_Data<'data_type, 'transition_type>
    | Skip_Transition of Skip_Transition_Message_Data<'data_type, 'transition_type>
    | Begin_Transition
    | Complete_Transition
    | Notify_Transition_Complete of int<runner_queue_item_id>
