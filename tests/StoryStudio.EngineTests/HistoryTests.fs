module HistoryTests

open Command_Types
open Runner_History
open Runner_Types_1
open Xunit

[<Fact>]
let ``sanitize_max_history_length clamps below min`` () =
    let result = sanitize_max_history_length -5
    Assert.Equal(0, result)

[<Fact>]
let ``sanitize_max_history_length clamps above max`` () =
    let result = sanitize_max_history_length 500
    Assert.Equal(99, result)

[<Fact>]
let ``truncate_history keeps newest entries`` () =
    let a = Runner_Saveable_State_Done Unchecked.defaultof<_>
    let b = Runner_Saveable_State_Done Unchecked.defaultof<_>
    let c = Runner_Saveable_State_Done Unchecked.defaultof<_>
    let history = [a; b; c]
    let truncated = truncate_history history 2
    Assert.Equal(2, truncated.Length)
    Assert.True(obj.ReferenceEquals(b, truncated.[0]))
    Assert.True(obj.ReferenceEquals(c, truncated.[1]))

[<Fact>]
let ``truncate_history keeps all entries when unlimited`` () =
    let history =
        [
            Runner_Saveable_State_Done Unchecked.defaultof<_>
            Runner_Saveable_State_Done Unchecked.defaultof<_>
            Runner_Saveable_State_Done Unchecked.defaultof<_>
        ]
    let truncated = truncate_history history 0
    Assert.Equal(history.Length, truncated.Length)

[<Fact>]
let ``undo and redo index helpers enforce bounds`` () =
    Assert.False(can_undo_for_index None)
    Assert.False(can_undo_for_index (Some 0))
    Assert.True(can_undo_for_index (Some 1))
    Assert.False(can_redo_for_index (Some 2) 3)
    Assert.True(can_redo_for_index (Some 1) 3)

[<Fact>]
let ``redo helper handles empty history gracefully`` () =
    Assert.False(can_redo_for_index None 0)
    Assert.False(can_redo_for_index (Some 0) 0)

[<Fact>]
let ``sanitize_max_history_length keeps in-range value`` () =
    let result = sanitize_max_history_length 20
    Assert.Equal(20, result)
