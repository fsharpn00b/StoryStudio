module Runner_Configuration

// console, window
open Browser.Dom
// IRefValue
open Feliz

open Configuration_Types
open Log
open Runner_Types_2

(* Debug *)

let debug_module_name = "Runner_Configuration"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions *)

(* TODO2 #configuration We do not use this for now.
The problem is this can change the current history index, but that is not reflected on the screen because we do not re-render. Then, if the player uses undo/redo, they appear to jump forward multiple steps.

For example:

History
0 1 2
  x <- current index

If the player sets the max history length to 1, indices 0 and 1 are truncated, and the current index becomes 2.

If we reinstate this, we should consider a version of truncate_history that can only truncate up to the current index.

For now, we just let any change to the maximum history length take effect after the player continues.
*)
(*
let private set_history_configuration
    (history : IRefValue<Runner_History>)
    (configuration : Runner_History_Configuration)
    : unit =

    let existing_history = history.current.history
    let next_history = truncate_history existing_history configuration.max_history_length 
    let removed_items = existing_history.Length - next_history.Length

    let next_current_index =
        match history.current.current_index with
        | None -> None
        | Some current_index ->
            if List.isEmpty next_history then None
            else Some (max 0 (current_index - removed_items))

    history.current <- {
        history.current with
            configuration = configuration
            current_index = next_current_index
            history = next_history
    }
(* Update the command menu in case undo or redo is no longer available. *)
    history.current.notify_history_changed ()
*)

(* This cannot go in Components/Configuration_Helpers.fs because it needs Runner_Component_Interfaces. *)
let set_configuration
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (history : IRefValue<Runner_History>)
    (old_configuration : IRefValue<Runner_Configuration>)
    (new_configuration : Runner_Configuration)
    : unit =
    do
        runner_component_interfaces.current.background.current.set_configuration new_configuration.background_configuration
        runner_component_interfaces.current.characters.current.set_configuration new_configuration.characters_configuration
        runner_component_interfaces.current.dialogue_box.current.set_configuration new_configuration.dialogue_box_configuration
        runner_component_interfaces.current.notifications.current.set_configuration new_configuration.temporary_notifications_configuration
(* For now, we just update the history configuration and let any change to the maximum history length take effect after the player continues. *)
//        set_history_configuration history new_configuration.history_configuration
        history.current <- { history.current with configuration = new_configuration.history_configuration }
        old_configuration.current <- new_configuration
(* TODO1 #configuration What about keyboard?
*)
