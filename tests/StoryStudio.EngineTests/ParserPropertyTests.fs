module ParserPropertyTests

open Command_Types
open Parser_2_Helpers
open Units_Of_Measure
open Xunit

let private mkToken (command : Command_Pre_Parse_Type) : Command_Pre_Parse_2 =
    {
        error_data = {
            source = "property-test"
            scene_id = 0<scene_id>
            script_text_index = 0
        }
        command = command
    }

let private standard_next_tokens : Command_Pre_Parse_Type list =
    [
        Command_Pre_Parse_Type.Command Music_Stop
        Command_Pre_Parse_Type.If "x > 0"
        Command_Pre_Parse_Type.Menu Unchecked.defaultof<_>
        Command_Pre_Parse_Type.Image_Map Unchecked.defaultof<_>
        Command_Pre_Parse_Type.End_Image_Map 0.5<seconds>
        Command_Pre_Parse_Type.Jump_Scene Unchecked.defaultof<_>
        Command_Pre_Parse_Type.Jump_Label "label_1"
        Command_Pre_Parse_Type.Jump_Internal Unchecked.defaultof<_>
        Command_Pre_Parse_Type.Label "label_2"
    ]

[<Fact>]
let ``standard next token cases always increment command ids by one`` () =
    for nextToken in standard_next_tokens do
        for currentId in [0<command_id>; 1<command_id>; 10<command_id>; 99<command_id>] do
            let result = get_next_command_id (mkToken nextToken) [] currentId
            Assert.Equal(currentId + 1<command_id>, result.next_id_for_command)
            Assert.Equal(currentId + 1<command_id>, result.next_available_id)

[<Fact>]
let ``branch control next token cases route to reserved end_if id`` () =
    let branchTokens =
        [
            Command_Pre_Parse_Type.Else_If "x"
            Command_Pre_Parse_Type.Else
            Command_Pre_Parse_Type.End_If
        ]

    for branchToken in branchTokens do
        for parentId in [0<command_id>; 2<command_id>; 7<command_id>] do
            for currentId in [5<command_id>; 10<command_id>; 25<command_id>] do
                let result = get_next_command_id (mkToken branchToken) [parentId] currentId
                Assert.Equal(parentId + 1<command_id>, result.next_id_for_command)
                Assert.Equal(currentId, result.next_available_id)

[<Fact>]
let ``valid branch transitions are accepted across permutations`` () =
    let branchStarters =
        [
            Command_Pre_Parse_Type.If "cond"
            Command_Pre_Parse_Type.Else_If "cond"
            Command_Pre_Parse_Type.Else
        ]
    let validFollowers =
        [
            Command_Pre_Parse_Type.Command Music_Stop
            Command_Pre_Parse_Type.If "nested"
        ]

    for starter in branchStarters do
        for follower in validFollowers do
            Assert.True(is_valid_branch_transition starter follower)
            check_current_token_and_next_token (mkToken starter) (mkToken follower)

[<Fact>]
let ``invalid branch transitions are rejected by pure validator`` () =
    let invalidFollowers =
        [
            Command_Pre_Parse_Type.Else
            Command_Pre_Parse_Type.Else_If "cond"
            Command_Pre_Parse_Type.End_If
            Command_Pre_Parse_Type.Menu Unchecked.defaultof<_>
            Command_Pre_Parse_Type.Label "end"
        ]

    for follower in invalidFollowers do
        Assert.False(is_valid_branch_transition (Command_Pre_Parse_Type.If "cond") follower)
        Assert.False(is_valid_branch_transition (Command_Pre_Parse_Type.Else_If "cond") follower)
        Assert.False(is_valid_branch_transition Command_Pre_Parse_Type.Else follower)
