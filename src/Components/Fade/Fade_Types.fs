module Fade_Types

open Units_Of_Measure

(* Types - public *)



type Fade_Configuration = unit

type Cross_Fade_Transition_State_Data<'T> = {
    old_data : 'T
    new_data : 'T
    transition_time : Fade_Transition_Time
    command_queue_item_id : int<command_queue_item_id>
}

type Fade_In_Transition_State_Data<'T> = {
    new_data : 'T
    transition_time : Fade_Transition_Time
    command_queue_item_id : int<command_queue_item_id>
}

type Fade_Out_Transition_State_Data<'T> = {
    old_data : 'T
    transition_time : Fade_Transition_Time
    command_queue_item_id : int<command_queue_item_id>
}

type Fade_State<'T> =
    | Idle_Hidden
    | Idle_Visible of 'T
    | Cross_Fade_Pre_Transition of Cross_Fade_Transition_State_Data<'T>
    | Cross_Fade_Transition of Cross_Fade_Transition_State_Data<'T>
    | Fade_In_Pre_Transition of Fade_In_Transition_State_Data<'T>
    | Fade_In_Transition of Fade_In_Transition_State_Data<'T>
    | Fade_Out_Pre_Transition of Fade_Out_Transition_State_Data<'T>
    | Fade_Out_Transition of Fade_Out_Transition_State_Data<'T>

type Fade_In_Message_Data<'T> = {
    new_data : 'T
    transition_time : Fade_Transition_Time
    command_queue_item_id : int<command_queue_item_id>
}

(* We get old_data from Idle_Visible. *)
type Fade_Out_Message_Data = {
    transition_time : Fade_Transition_Time
    command_queue_item_id : int<command_queue_item_id>
}

(* We get old_data from Idle_Visible. *)
type Cross_Fade_Message_Data<'T> = {
    new_data : 'T
    transition_time : Fade_Transition_Time
    command_queue_item_id : int<command_queue_item_id>
}

type Show_Message_Data<'T> = {
    data : 'T
    is_notify_transition_complete : bool
    command_queue_item_id : int<command_queue_item_id> option
}

type Hide_Message_Data<'T> = {
    is_notify_transition_complete : bool
    command_queue_item_id : int<command_queue_item_id> option
}

type Fade_Message<'T> =
    | Show of Show_Message_Data<'T>
    | Hide of Hide_Message_Data<'T>
    | Fade_Out of Fade_Out_Message_Data
    | Fade_In of Fade_In_Message_Data<'T>
    | Cross_Fade of Cross_Fade_Message_Data<'T>
    | Begin_Fade_Transition
    | Complete_Fade_Transition
    | Notify_Transition_Complete of int<command_queue_item_id>
