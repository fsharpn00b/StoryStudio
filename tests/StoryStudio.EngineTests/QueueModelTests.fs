module QueueModelTests

open Command_Types
open InvariantAssertions
open JavaScript_Interop_1
open Runner_Queue_Transition
open Runner_Types_1
open Runner_Types_2
open Units_Of_Measure
open Xunit

let private mkNextCommandData : Runner_Queue_Next_Command_Data option =
    Some {
        next_command_queue_item_id = 4<command_queue_item_id>
        next_command_data = {
            next_command_scene_id = 1<scene_id>
            next_command_id = 2<command_id>
        }
    }

let private mkQueueData
    (continue_after_finished : bool)
    (add_to_history : bool)
    (autosave : bool)
    (next_command_data : Runner_Queue_Next_Command_Data option)
    (menu_variables : Menu_Variables)
    : Runner_Queue_State_Running_Data =
    {
        commands = Map.empty
        next_command_data = next_command_data
        components_used_by_commands = Set.empty
        continue_after_finished = continue_after_finished
        add_to_history = add_to_history
        autosave = autosave
        menu_variables = menu_variables
    }

let private mkQueueItem : Runner_Queue_Item =
    {
        command_data = {
            command = None
            error_data = {
                source = "model-test"
                scene_id = 1<scene_id>
                script_text_index = 0
            }
            behavior = Continue_Immediately { run_queue_now = false; autosave = false }
            components_used = Set.empty
            next_command_data = None
        }
        order_in_queue = 0<command_queue_order>
        components_used_by_command = Set.empty
    }

type private QueueModelResult = {
    next_queue_is_idle : bool
    should_add_to_history : bool
    should_autosave : bool
    should_continue_after_finished : bool
}

let private compute_model_transition_completion
    (queue_data : Runner_Queue_State_Running_Data)
    (remaining_commands : Runner_Queue_Command_Map)
    : QueueModelResult =

    if Map.isEmpty remaining_commands then
        {
            next_queue_is_idle = true
            should_add_to_history = queue_data.add_to_history
            should_autosave = queue_data.autosave
            should_continue_after_finished = queue_data.continue_after_finished
        }
    else
        {
            next_queue_is_idle = false
            should_add_to_history = false
            should_autosave = false
            should_continue_after_finished = false
        }

[<Fact>]
let ``compute_transition_completion matches reference model across scenario matrix`` () =
    let scenarios =
        [
            true, true, true
            true, false, true
            false, true, false
            false, false, false
        ]
    let commandMaps =
        [
            Map.empty
            Map.ofList [1<command_queue_item_id>, mkQueueItem]
        ]
    let queueStates : Runner_Queue list =
        [
            Queue_Running (mkQueueData true true true mkNextCommandData Map.empty)
            Queue_Interrupting (mkQueueData true true true mkNextCommandData Map.empty)
        ]
    let nextCommandDataCases =
        [
            mkNextCommandData
            None
        ]
    let menuVariableCases : Menu_Variables list =
        [
            Map.empty
            (Map.ofList [ "direction", 1 ] : Menu_Variables)
        ]

    for continueAfterFinished, addToHistory, autosave in scenarios do
        for nextCommandData in nextCommandDataCases do
            for menuVariables in menuVariableCases do
                let queueData = mkQueueData continueAfterFinished addToHistory autosave nextCommandData menuVariables
                for commandMap in commandMaps do
                    let expected = compute_model_transition_completion queueData commandMap
                    for queueState in queueStates do
                        let actual = compute_transition_completion queueState queueData commandMap
                        assert_transition_result_shape commandMap actual
                        Assert.Equal(expected.should_add_to_history, actual.should_add_to_history)
                        Assert.Equal(expected.should_autosave, actual.should_autosave)
                        Assert.Equal(expected.should_continue_after_finished, actual.should_continue_after_finished)
                        match actual.next_queue with
                        | Queue_Idle idleData ->
                            Assert.True(expected.next_queue_is_idle)
                            Assert.Equal(nextCommandData, idleData.next_command_data)
                            Assert.Equal<Menu_Variables>(menuVariables, idleData.menu_variables)
                        | Queue_Running runningData ->
                            Assert.False(expected.next_queue_is_idle)
                            Assert.Equal<Menu_Variables>(menuVariables, runningData.menu_variables)
                        | Queue_Interrupting interruptingData ->
                            Assert.False(expected.next_queue_is_idle)
                            Assert.Equal<Menu_Variables>(menuVariables, interruptingData.menu_variables)
                        | _ -> failwith "Unexpected queue state."

[<Fact>]
let ``queue state type is preserved for non-empty command completion`` () =
    let queueData = mkQueueData true true true mkNextCommandData Map.empty
    let commands =
        Map.ofList [
            1<command_queue_item_id>, mkQueueItem
            2<command_queue_item_id>, { mkQueueItem with order_in_queue = 1<command_queue_order> }
        ]
    let runningResult = compute_transition_completion (Queue_Running queueData) queueData commands
    let interruptingResult = compute_transition_completion (Queue_Interrupting queueData) queueData commands
    match runningResult.next_queue with
    | Queue_Running _ -> ()
    | _ -> failwith "Expected Queue_Running."
    match interruptingResult.next_queue with
    | Queue_Interrupting _ -> ()
    | _ -> failwith "Expected Queue_Interrupting."
