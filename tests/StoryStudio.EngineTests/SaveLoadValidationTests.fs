module SaveLoadValidationTests

open System
open Save_Load_Validation
open Xunit

[<Fact>]
let ``validate_saved_game_name accepts regular names`` () =
    match validate_saved_game_name "chapter_01" with
    | Ok () -> ()
    | Error (message, _) -> failwithf "Expected valid name, got: %s" message

[<Fact>]
let ``validate_saved_game_name rejects invalid characters`` () =
    match validate_saved_game_name "chapter 01" with
    | Ok () -> failwith "Expected failure for invalid name."
    | Error _ -> ()

[<Fact>]
let ``validate_javascript_state rejects empty input`` () =
    match validate_javascript_state "" with
    | Ok () -> failwith "Expected failure for empty JavaScript state."
    | Error _ -> ()

[<Fact>]
let ``validate_javascript_state rejects oversized payload`` () =
    let large_payload = String.replicate 2000001 "x"
    match validate_javascript_state large_payload with
    | Ok () -> failwith "Expected failure for oversized JavaScript state."
    | Error _ -> ()

[<Fact>]
let ``validate_import_file_contents rejects empty file`` () =
    match validate_import_file_contents "" with
    | Ok _ -> failwith "Expected failure for empty file."
    | Error _ -> ()
