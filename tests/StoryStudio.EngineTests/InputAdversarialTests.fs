module InputAdversarialTests

open Command_Types
open InvariantAssertions
open Runner_History
open Runner_Queue_Transition
open Runner_Types_2
open Units_Of_Measure
open Xunit

let private mkQueueData : Runner_Queue_State_Running_Data =
    {
        commands = Map.empty
        next_command_data = None
        components_used_by_commands = Set.empty
        continue_after_finished = false
        add_to_history = true
        autosave = true
        menu_variables = Map.empty
    }

let private mkQueueItem : Runner_Queue_Item =
    {
        command_data = {
            command = None
            error_data = {
                source = "input-adversarial"
                scene_id = 0<scene_id>
                script_text_index = 0
            }
            behavior = Continue_Immediately { run_queue_now = false; autosave = false }
            components_used = Set.empty
            next_command_data = None
        }
        order_in_queue = 0<command_queue_order>
        components_used_by_command = Set.empty
    }

type private PlayerAction =
    | Undo
    | Redo
    | CompleteTransitionWithRemainingCommand
    | CompleteTransitionToIdle

type private ReplayState = {
    history_index : int option
    queue_is_idle : bool
    queue_has_commands : bool
}

let private apply_history_action (current_index : int option) (history_length : int) (action : PlayerAction) =
    match action with
    | Undo ->
        if can_undo_for_index current_index then
            current_index |> Option.map (fun idx -> idx - 1)
        else current_index
    | Redo ->
        if can_redo_for_index current_index history_length then
            current_index |> Option.map (fun idx -> idx + 1)
        else current_index
    | _ -> current_index

let private apply_replay_action (state : ReplayState) (action : PlayerAction) : ReplayState =
    let nextHistoryIndex = apply_history_action state.history_index 5 action
    let remainingCommands =
        match action with
        | CompleteTransitionWithRemainingCommand ->
            Map.ofList [1<command_queue_item_id>, mkQueueItem]
        | _ -> Map.empty
    let queueState =
        if state.queue_is_idle then Queue_Running mkQueueData
        else Queue_Interrupting mkQueueData
    let transitionResult = compute_transition_completion queueState mkQueueData remainingCommands
    let queueIsIdle, queueHasCommands =
        match transitionResult.next_queue with
        | Queue_Idle _ -> true, false
        | Queue_Running data
        | Queue_Interrupting data -> false, not (Map.isEmpty data.commands)
        | _ -> false, false
    {
        history_index = nextHistoryIndex
        queue_is_idle = queueIsIdle
        queue_has_commands = queueHasCommands
    }

[<Fact>]
let ``rapid undo redo sequence keeps index in bounds`` () =
    let actions =
        [
            Undo; Undo; Redo; Redo; Redo; Undo; Redo; Undo; Undo; Undo
            Redo; Undo; Redo; Redo; Undo; Redo; Undo; Redo
        ]

    let history_length = 5
    let mutable current_index = Some 2

    for action in actions do
        current_index <- apply_history_action current_index history_length action
        assert_history_index_in_bounds current_index history_length

[<Fact>]
let ``undo redo sequence is stable with minimal history length`` () =
    let history_length = 1
    let mutable current_index = Some 0
    for action in [Undo; Redo; Undo; Redo; Undo; Redo] do
        current_index <- apply_history_action current_index history_length action
        assert_history_index_in_bounds current_index history_length
        Assert.Equal(Some 0, current_index)

[<Fact>]
let ``rapid mixed transition completions never produce impossible queue states`` () =
    let actions =
        [
            CompleteTransitionWithRemainingCommand
            CompleteTransitionToIdle
            CompleteTransitionWithRemainingCommand
            CompleteTransitionWithRemainingCommand
            CompleteTransitionToIdle
            CompleteTransitionToIdle
            CompleteTransitionWithRemainingCommand
        ]

    for action in actions do
        let remainingCommands =
            match action with
            | CompleteTransitionWithRemainingCommand ->
                Map.ofList [1<command_queue_item_id>, mkQueueItem]
            | _ -> Map.empty
        let currentQueueState =
            match action with
            | CompleteTransitionWithRemainingCommand -> Queue_Running mkQueueData
            | _ -> Queue_Interrupting mkQueueData
        let result = compute_transition_completion currentQueueState mkQueueData remainingCommands
        assert_transition_result_shape remainingCommands result

[<Fact>]
let ``deterministic replay of mixed player actions converges to identical final state`` () =
    let actions =
        [
            CompleteTransitionWithRemainingCommand
            Undo
            Redo
            CompleteTransitionToIdle
            Undo
            CompleteTransitionWithRemainingCommand
            Redo
            CompleteTransitionToIdle
            Undo
            Redo
            CompleteTransitionToIdle
        ]
    let initialState = {
        history_index = Some 2
        queue_is_idle = true
        queue_has_commands = false
    }
    let finalStateA = (initialState, actions) ||> List.fold apply_replay_action
    let finalStateB = (initialState, actions) ||> List.fold apply_replay_action
    Assert.Equal(finalStateA, finalStateB)
