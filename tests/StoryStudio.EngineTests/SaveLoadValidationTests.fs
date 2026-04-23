module SaveLoadValidationTests

open System
open Save_Load_Types
open Save_Load_Validation
open Units_Of_Measure
open Xunit

[<Literal>]
let private validScreenshot = "data:image/jpeg;base64,AA=="

let private mkSavedGame
    (name : string)
    (screenshot : string)
    (timestamp : DateTime)
    (runnerSaveableStateJson : string)
    : Existing_Saved_Game =
    {
        id = 1<saved_game_id>
        name = name
        screenshot = screenshot
        timestamp = timestamp
        runner_saveable_state_json = runnerSaveableStateJson
    }

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

[<Fact>]
let ``validate_saved_game_name rejects oversized name`` () =
    let oversized = String.replicate 41 "a"
    match validate_saved_game_name oversized with
    | Ok () -> failwith "Expected failure for oversized name."
    | Error _ -> ()

[<Fact>]
let ``validate_saved_game rejects invalid screenshot format`` () =
    let game = mkSavedGame "slot1" "invalid-screenshot" DateTime.UtcNow "{}"
    match validate_saved_game game with
    | Ok _ -> failwith "Expected invalid screenshot format failure."
    | Error _ -> ()

[<Fact>]
let ``validate_saved_game rejects oversized screenshot`` () =
    let oversizedScreenshot = "data:image/jpeg;base64," + String.replicate 2_000_001 "A"
    let game = mkSavedGame "slot1" oversizedScreenshot DateTime.UtcNow "{}"
    match validate_saved_game game with
    | Ok _ -> failwith "Expected oversized screenshot failure."
    | Error _ -> ()

[<Fact>]
let ``validate_saved_game rejects oversized state payload`` () =
    let oversizedState = String.replicate 2_000_001 "x"
    let game = mkSavedGame "slot1" validScreenshot DateTime.UtcNow oversizedState
    match validate_saved_game game with
    | Ok _ -> failwith "Expected oversized state payload failure."
    | Error _ -> ()

[<Fact>]
let ``validate_saved_games rejects empty list`` () =
    match validate_saved_games [] with
    | Ok () -> failwith "Expected failure for empty list."
    | Error _ -> ()

[<Fact>]
let ``validate_saved_games rejects more than max imported items`` () =
    let tooMany =
        [1..101]
        |> List.map (fun i ->
            mkSavedGame $"slot{i}" validScreenshot DateTime.UtcNow "{}"
        )
    match validate_saved_games tooMany with
    | Ok () -> failwith "Expected failure for too many saved games."
    | Error _ -> ()

[<Fact>]
let ``validate_saved_games reports per-entry validation errors`` () =
    let games =
        [
            mkSavedGame "slot1" "bad-screenshot" DateTime.UtcNow "{}"
        ]
    match validate_saved_games games with
    | Ok () -> failwith "Expected failure for invalid entry."
    | Error (message, _) -> Assert.Contains("Could not validate saved game", message)

[<Fact>]
let ``validate_import_file_contents rejects oversized file`` () =
    let oversized = String.replicate 5_000_001 "x"
    match validate_import_file_contents oversized with
    | Ok _ -> failwith "Expected oversized file failure."
    | Error _ -> ()

