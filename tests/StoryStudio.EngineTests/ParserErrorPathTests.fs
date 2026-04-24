module ParserErrorPathTests

open Command_Types
open Parser_1_Helpers
open Parser_2_Helpers
open Units_Of_Measure
open Xunit

[<Fact>]
let ``unknown jump scene destination returns none`` () =
    let scripts =
        [
            { id = 0<scene_id>; name = "start"; content = "" }
            { id = 1<scene_id>; name = "day_1"; content = "" }
        ]
    let result = get_jump_scene_destination scripts "missing_scene"
    Assert.True(result.IsNone)

[<Fact>]
let ``known jump scene destination resolves to scene id`` () =
    let scripts =
        [
            { id = 0<scene_id>; name = "start"; content = "" }
            { id = 5<scene_id>; name = "chapter_5"; content = "" }
        ]
    let result = get_jump_scene_destination scripts "chapter_5"
    Assert.Equal(Some 5<scene_id>, result)

[<Fact>]
let ``terminal token validator rejects incomplete branch tokens`` () =
    Assert.False(is_valid_terminal_token (Command_Pre_Parse_Type.If "x"))
    Assert.False(is_valid_terminal_token (Command_Pre_Parse_Type.Else_If "x"))
    Assert.False(is_valid_terminal_token Command_Pre_Parse_Type.Else)

[<Fact>]
let ``terminal token validator accepts complete command tokens`` () =
    Assert.True(is_valid_terminal_token (Command_Pre_Parse_Type.Command Music_Stop))
    Assert.True(is_valid_terminal_token (Command_Pre_Parse_Type.Label "done"))
    Assert.True(is_valid_terminal_token (Command_Pre_Parse_Type.End_If))

[<Fact>]
let ``jump label destination resolution returns expected id when present`` () =
    let labels = Map.ofList [ "a", 1<command_id>; "b", 8<command_id> ]
    let resolved = resolve_jump_label_destination labels "b"
    Assert.Equal(Some 8<command_id>, resolved)

[<Fact>]
let ``jump label destination resolution returns none when missing`` () =
    let labels = Map.ofList [ "a", 1<command_id> ]
    let resolved = resolve_jump_label_destination labels "missing"
    Assert.True(resolved.IsNone)

[<Fact>]
let ``label conflict with scene name raises parser semantics error`` () =
    let scripts = [ { id = 0<scene_id>; name = "start"; content = "" } ]
    Assert.Throws<Parsing_Semantics_Error>(fun () -> check_label scripts "start" 0 |> ignore) |> ignore
