module ParserMatrixTests

open Command_Types
open Runner_Queue_Helpers_1
open Parser_2_Helpers
open Runner_Types_2
open Units_Of_Measure
open Xunit

let private mkToken (command : Command_Pre_Parse_Type) : Command_Pre_Parse_2 =
    {
        error_data = {
            source = "test"
            scene_id = 0<scene_id>
            script_text_index = 0
        }
        command = command
    }

[<Fact>]
let ``commands that transition wait for callback and continue afterward`` () =
    let transitionCommands =
        [
            Dialogue_Box_Show
            Dialogue_Box_Hide
            Fade_Out_All 1.0<seconds>
        ]

    for command in transitionCommands do
        match command_to_behavior command with
        | Wait_For_Callback behavior ->
            Assert.True(behavior.continue_afterward)
            Assert.False(behavior.add_to_history)
        | _ -> failwith "Expected Wait_For_Callback behavior."

[<Fact>]
let ``javascript inline command runs immediately and flushes queue`` () =
    let behavior =
        command_to_behavior (
            JavaScript_Inline {
                code = "window.state.count += 1;"
                script_text_index = 0
            }
        )
    match behavior with
    | Continue_Immediately continueData ->
        Assert.True(continueData.run_queue_now)
        Assert.False(continueData.autosave)
    | _ -> failwith "Expected Continue_Immediately behavior."

[<Fact>]
let ``dialogue uses dialogue component only`` () =
    let componentIds =
        command_to_component_ids (
            Dialogue {
                character_short_name = "l"
                character_full_name = "Laura"
                text = "Hello"
                javascript_interpolations = []
            }
        )
    Assert.Equal<Set<Runner_Component_Names>>(set [Dialogue_Box], componentIds)

[<Fact>]
let ``fade out all uses background characters and dialogue components`` () =
    let componentIds = command_to_component_ids (Fade_Out_All 0.5<seconds>)
    Assert.Equal<Set<Runner_Component_Names>>(set [Background; Characters; Dialogue_Box], componentIds)

[<Fact>]
let ``next command id increments for standard commands`` () =
    let nextToken = mkToken (Command_Pre_Parse_Type.Command Music_Stop)
    let result = get_next_command_id nextToken [] 4<command_id>
    Assert.Equal(5<command_id>, result.next_id_for_command)
    Assert.Equal(5<command_id>, result.next_available_id)

[<Fact>]
let ``next command id jumps to parent end_if slot for branch controls`` () =
    let parentStack = [10<command_id>]
    let result = get_next_command_id (mkToken Command_Pre_Parse_Type.End_If) parentStack 18<command_id>
    Assert.Equal(11<command_id>, result.next_id_for_command)
    Assert.Equal(18<command_id>, result.next_available_id)
