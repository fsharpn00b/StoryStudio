module QueueTransitionTests

open Command_Types
open InvariantAssertions
open Runner_Queue_Transition
open Runner_Types_1
open Runner_Types_2
open Units_Of_Measure
open Xunit

let private mkNextCommandData : Runner_Queue_Next_Command_Data option =
    Some {
        next_command_queue_item_id = LanguagePrimitives.Int32WithMeasure<command_queue_item_id> 4
        next_command_data = {
            next_command_scene_id = LanguagePrimitives.Int32WithMeasure<scene_id> 1
            next_command_id = LanguagePrimitives.Int32WithMeasure<command_id> 2
        }
    }

let private mkQueueData : Runner_Queue_State_Running_Data =
    {
        commands = Map.empty
        next_command_data = mkNextCommandData
        components_used_by_commands = Set.empty
        continue_after_finished = true
        add_to_history = true
        autosave = true
        menu_variables = Map.empty
    }

let private mkQueueDataWithFlags
    (continueAfterFinished : bool)
    (addToHistory : bool)
    (autosave : bool)
    : Runner_Queue_State_Running_Data =
    { mkQueueData with
        continue_after_finished = continueAfterFinished
        add_to_history = addToHistory
        autosave = autosave
    }

let private mkQueueItem : Runner_Queue_Item =
    {
        command_data = {
            command = None
            error_data = {
                source = "test"
                scene_id = LanguagePrimitives.Int32WithMeasure<scene_id> 1
                script_text_index = 0
            }
            behavior = Continue_Immediately { run_queue_now = false; autosave = false }
            components_used = Set.empty
            next_command_data = None
        }
        order_in_queue = LanguagePrimitives.Int32WithMeasure<command_queue_order> 0
        components_used_by_command = Set.empty
    }

[<Fact>]
let ``completion with empty commands returns idle and follow-up flags`` () =
    let result = compute_transition_completion (Queue_Running mkQueueData) mkQueueData Map.empty
    assert_transition_result_shape Map.empty result
    match result.next_queue with
    | Queue_Idle idleData ->
        Assert.Equal(mkQueueData.next_command_data, idleData.next_command_data)
        Assert.True(result.should_add_to_history)
        Assert.True(result.should_autosave)
        Assert.True(result.should_continue_after_finished)
    | _ -> failwith "Expected Queue_Idle."

[<Fact>]
let ``completion with running queue preserves running state`` () =
    let commands = Map.ofList [LanguagePrimitives.Int32WithMeasure<command_queue_item_id> 1, mkQueueItem]
    let result = compute_transition_completion (Queue_Running mkQueueData) mkQueueData commands
    assert_transition_result_shape commands result
    match result.next_queue with
    | Queue_Running runningData ->
        Assert.Equal(1, runningData.commands.Count)
        Assert.False(result.should_add_to_history)
        Assert.False(result.should_autosave)
        Assert.False(result.should_continue_after_finished)
    | _ -> failwith "Expected Queue_Running."

[<Fact>]
let ``completion with interrupting queue preserves interrupting state`` () =
    let commands = Map.ofList [LanguagePrimitives.Int32WithMeasure<command_queue_item_id> 1, mkQueueItem]
    let result = compute_transition_completion (Queue_Interrupting mkQueueData) mkQueueData commands
    assert_transition_result_shape commands result
    match result.next_queue with
    | Queue_Interrupting runningData ->
        Assert.Equal(1, runningData.commands.Count)
    | _ -> failwith "Expected Queue_Interrupting."

[<Fact>]
let ``empty command completion mirrors flag combinations`` () =
    let scenarios =
        [
            true, true, true
            true, false, true
            false, true, false
            false, false, false
        ]

    for continueAfterFinished, addToHistory, autosave in scenarios do
        let data = mkQueueDataWithFlags continueAfterFinished addToHistory autosave
        let result = compute_transition_completion (Queue_Running data) data Map.empty
        Assert.Equal(addToHistory, result.should_add_to_history)
        Assert.Equal(autosave, result.should_autosave)
        Assert.Equal(continueAfterFinished, result.should_continue_after_finished)

[<Fact>]
let ``non-empty completion always suppresses follow-up side effects`` () =
    let scenarios =
        [
            true, true, true
            true, false, true
            false, true, false
            false, false, false
        ]
    let commands = Map.ofList [1<command_queue_item_id>, mkQueueItem]

    for continueAfterFinished, addToHistory, autosave in scenarios do
        let data = mkQueueDataWithFlags continueAfterFinished addToHistory autosave
        let result = compute_transition_completion (Queue_Running data) data commands
        Assert.False(result.should_add_to_history)
        Assert.False(result.should_autosave)
        Assert.False(result.should_continue_after_finished)
