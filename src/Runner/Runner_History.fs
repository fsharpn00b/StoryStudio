module Runner_History

// console, window
open Browser.Dom
// IRefValue
open Feliz

open Log
open Runner_State
open Runner_Transition
open Runner_Types

(* Debug *)

let debug_module_name = "Runner_History"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Functions *)

let add_to_history
    (runner_state : Runner_State)
    (history : IRefValue<Runner_History>)
    : unit =

    do
        history.current <-
            match history.current.current_index with
            | None ->
                {
                    history.current with
                        current_index = Some 0
                        history = [get_state runner_state]
                }
            | Some current_index ->
                {
                    history.current with
                        current_index = Some <| current_index + 1
                        history = (List.take (current_index + 1) history.current.history) @ [get_state runner_state]
                }
        history.current.notify_history_changed ()

let clear_history
    (history : IRefValue<Runner_History>)
    : unit =

    do
        history.current <- { history.current with current_index = None; history = [] }
        history.current.notify_history_changed ()

let can_undo
    (history : IRefValue<Runner_History>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : bool =

(* It seems this function can be called before Configuration or Save_Load is initialized. *)
    if
        Unchecked.defaultof<_> = runner_component_interfaces.current.configuration.current ||
        Unchecked.defaultof<_> = runner_component_interfaces.current.save_load.current then false
    elif runner_component_interfaces.current.configuration.current.is_visible () ||
        runner_component_interfaces.current.save_load.current.is_visible () then
        false
    else
        match history.current.current_index with
        | Some current_index_1 -> current_index_1 > 0
        | None -> false

let can_redo
    (history : IRefValue<Runner_History>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    : bool =

(* It seems this function can be called before Configuration or Save_Load is initialized. *)
    if
        Unchecked.defaultof<_> = runner_component_interfaces.current.configuration.current ||
        Unchecked.defaultof<_> = runner_component_interfaces.current.save_load.current then false
    elif runner_component_interfaces.current.configuration.current.is_visible () ||
        runner_component_interfaces.current.save_load.current.is_visible () then
        false
    else
        match history.current.current_index with
        | Some current_index_1 -> current_index_1 < history.current.history.Length - 1
        | None -> false

(* TODO2 We have not tried to redo during a transition. That is possible, as we only truncate the history when we add to it. In other words, we could:
1 Run a command (a) that uses a transition.
2 Reach a pause point (b).
3 Undo to (a).
4 Click to progress from (a) to (b), running the transaction again.
5 Redo to (b), which should still be in our history.
(end)
*)
let undo_redo
    (runner_state : Runner_State)
    (history : IRefValue<Runner_History>)
    (undo : bool)
    : unit =

    #if debug
    do debug "undo_redo" String.Empty ["History index (before)", history.current.current_index; "History length", history.current.history.Length]
    #endif

    match history.current.current_index with
    | Some current_index_1 ->
        if (undo && current_index_1 > 0) || (not undo && current_index_1 < history.current.history.Length - 1) then
(* We do not call force_complete_transitions () until we have determined undo/redo is valid. *)
            do force_complete_transitions runner_state true (fun () ->
                #if debug
                do debug "undo_redo" "Runner_State.force_complete_transitions () done." []
                #endif

                let current_index_2 = if undo then current_index_1 - 1 else current_index_1 + 1
                do
                    history.current <- { history.current with current_index = Some current_index_2 }
                    history.current.notify_history_changed ()

                    #if debug
                    debug "undo_redo" String.Empty ["History index (after)", history.current.current_index; "History length", history.current.history.Length]
                    #endif

                    set_state runner_state history.current.history.[current_index_2]
            )
    | None -> ()
