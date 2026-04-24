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
