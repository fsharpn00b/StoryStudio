module ParserBehaviorTests

open Command_Types
open Parser_2_Helpers
open Runner_Queue_Helpers_1
open Units_Of_Measure
open Xunit

let private mkCommand (command : Command_Pre_Parse_Type) : Command_Pre_Parse_2 =
    {
        error_data = {
            source = "test"
            scene_id = LanguagePrimitives.Int32WithMeasure<scene_id> 0
            script_text_index = 0
        }
        command = command
    }

[<Fact>]
let ``If followed by dialogue is valid`` () =
    let current = mkCommand (Command_Pre_Parse_Type.If "x")
    let next =
        mkCommand (
            Command_Pre_Parse_Type.Command (
                Dialogue {
                    character_short_name = "l"
                    character_full_name = "Laura"
                    text = "Hi"
                    javascript_interpolations = []
                }
            )
        )
    check_current_token_and_next_token current next

[<Fact>]
let ``Branch control token points to reserved End_If id`` () =
    let result =
        get_next_command_id
            (mkCommand Command_Pre_Parse_Type.Else)
            [LanguagePrimitives.Int32WithMeasure<command_id> 10]
            (LanguagePrimitives.Int32WithMeasure<command_id> 15)
    Assert.Equal(LanguagePrimitives.Int32WithMeasure<command_id> 11, result.next_id_for_command)
    Assert.Equal(LanguagePrimitives.Int32WithMeasure<command_id> 15, result.next_available_id)

[<Fact>]
let ``Dialogue commands wait for callback and add history`` () =
    let behavior =
        command_to_behavior (
            Dialogue {
                character_short_name = "l"
                character_full_name = "Laura"
                text = "Hello"
                javascript_interpolations = []
            }
        )
    match behavior with
    | Wait_For_Callback data ->
        Assert.False(data.continue_afterward)
        Assert.True(data.add_to_history)
        Assert.False(data.autosave)
    | _ -> failwith "Expected Wait_For_Callback behavior."
