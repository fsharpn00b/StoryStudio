module InvariantAssertions

open Runner_Queue_Transition
open Runner_Types_2
open Xunit

let assert_transition_result_shape
    (remaining_commands : Runner_Queue_Command_Map)
    (result : Transition_Completion_Result)
    : unit =

    if Map.isEmpty remaining_commands then
        match result.next_queue with
        | Queue_Idle _ -> ()
        | _ -> failwith "Expected Queue_Idle when no commands remain."
    else
        match result.next_queue with
        | Queue_Running data
        | Queue_Interrupting data ->
            Assert.Equal(remaining_commands.Count, data.commands.Count)
            Assert.False(result.should_add_to_history)
            Assert.False(result.should_autosave)
            Assert.False(result.should_continue_after_finished)
        | _ -> failwith "Expected running/interrupting queue when commands remain."

let assert_history_index_in_bounds
    (current_index : int option)
    (history_length : int)
    : unit =

    match current_index with
    | None -> Assert.True(history_length >= 0)
    | Some idx ->
        Assert.True(idx >= 0)
        Assert.True(idx < history_length || history_length = 0)
