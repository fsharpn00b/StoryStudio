module Runner_State

// console, window
open Browser.Dom
// IRefValue
open Feliz

open JavaScript_Interop_2
open Log
open Runner_Types

(* Debug *)

let debug_module_name = "Runner_State"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions - state *)

let get_state
    (runner_state : Runner_State)
(* This refers to a parameter we have since removed, but the point stands.
Previously this parameter was type Command_State rather than IRefValue<Command_State>, meaning we called .current before passing the result to this function. It seems that caused this function to see a stale state. It seems the rule is, with any IRefValue, we must call .current as late as possible, whether setting or getting or the value. *)
    : Runner_Saveable_State =

    match runner_state.queue.current with
    | Queue_Idle data ->

        Runner_Saveable_State_Running {
            next_command = data.next_command
(* We clear the history when we load a saved game. When we load a saved game, we need to determine whether to re-add the current state to the history.
We complete all running transitions before we show the save/load game screen. Any commands with behavior Continue_Immediately should have finished. In effect, the queue should have been in a Wait_For_Callback state. So when we load a saved game, we only need to check add_to_history, which is simply the inverse of continue_after_finished. We do not use continue_after_finished itself because we need to set it to false to halt running commands when the user opens the save/load game screen or rolls back/forward.
If add_to_history is false, which means that when we halted running commands, we would otherwise have run the next command without waiting for input from the user, we should not add the current state to the history.
*)
            add_to_history = data.add_to_history
            autosave = data.autosave
            component_data = {
                background = runner_state.runner_component_interfaces.current.background.current.get_state ()
                dialogue = runner_state.runner_component_interfaces.current.dialogue_box.current.get_state ()
                characters = runner_state.runner_component_interfaces.current.characters.current.get_state ()
                menu = runner_state.runner_component_interfaces.current.menu.current.get_state ()
                image_map = runner_state.runner_component_interfaces.current.image_map.current.get_state ()
                music = runner_state.runner_component_interfaces.current.music.current.get_state ()
                notifications = runner_state.runner_component_interfaces.current.notifications.current.get_state ()
                javascript = get_state_from_js ()
            }
            menu_variables = data.menu_variables
        }

    | Queue_Done ->

        Runner_Saveable_State_Done {
            background = runner_state.runner_component_interfaces.current.background.current.get_state ()
            dialogue = runner_state.runner_component_interfaces.current.dialogue_box.current.get_state ()
            characters = runner_state.runner_component_interfaces.current.characters.current.get_state ()
            menu = runner_state.runner_component_interfaces.current.menu.current.get_state ()
            image_map = runner_state.runner_component_interfaces.current.image_map.current.get_state ()
            music = runner_state.runner_component_interfaces.current.music.current.get_state ()
            notifications = runner_state.runner_component_interfaces.current.notifications.current.get_state ()
            javascript = get_state_from_js ()
        }

(* At this point, we should have forced transitions to complete, or already have been in state Queue_Idle. *)
    | _ -> error "get_state" "Unexpected queue state." ["Queue state", runner_state.queue.current] |> invalidOp

(* Each UI component is responsible to notify its container (Runner) that a transition is complete. This is because some components (Characters, Dialogue_Box) might handle multiple transitions (multiple characters, fade and typing transitions, respectively). We want the component to send only a single notification for these grouped transitions, because Runner does not concern itself with the individual transitions - those should be contained by the component.
*)
let set_state
    (runner_state : Runner_State)
    (runner_saveable_state : Runner_Saveable_State)
    : unit =

    let component_data =
        match runner_saveable_state with
        | Runner_Saveable_State_Running data -> data.component_data
        | Runner_Saveable_State_Done data -> data

    do
(* See the notes in Runner.fs, case 3, for how this works. *)
(* A UI component is responsible for completing its transitions before setting its state. *)
(* TODO2 Consider calling Runner_Run.force_complete_transitions () here instead.
- Either way, do we need a delay or callback after each component completes its transition, as we use callbacks for Runner_Run.force_complete_transitions ()/get_next_command ()?
*)
(* TODO2 When we save (that is, call each component's get_state () method), we record the final state of any in-progress transition, rather than the original state.
We might want to get the state at the most recent pausable point (which might be the current point if no transition is in progress). Since we plan to represent history and save state the same way, this should be feasible. However, if they have just loaded the game, the history might be empty.
*)
        runner_state.runner_component_interfaces.current.background.current.set_state component_data.background
        runner_state.runner_component_interfaces.current.dialogue_box.current.set_state component_data.dialogue
        runner_state.runner_component_interfaces.current.characters.current.set_state component_data.characters
        runner_state.runner_component_interfaces.current.menu.current.set_state component_data.menu
        runner_state.runner_component_interfaces.current.image_map.current.set_state component_data.image_map
        runner_state.runner_component_interfaces.current.music.current.set_state component_data.music
        runner_state.runner_component_interfaces.current.notifications.current.set_state component_data.notifications
        set_state_in_js_with_exception component_data.javascript

(* We set the command state afterward to prevent having it overwritten due to a component completing an existing transition (on its own, not because we called its set_state () method) after we set the command state but before we call the components' set_state () methods.
*)
        runner_state.queue.current <-
            match runner_saveable_state with
            | Runner_Saveable_State_Running data ->
                Queue_Idle {
                    next_command = data.next_command
                    add_to_history = data.add_to_history
                    autosave = data.autosave
                    menu_variables = data.menu_variables
                }
            | Runner_Saveable_State_Done _ -> Queue_Done

(* TODO2 Should we call get_next_command () at the end of set_state ()?
See also TODO 2 item at the end of Runner_Run.fs.

N Maybe, when we save the game, we should check the history and retreat to the last pause point.
20251109 We don't want to now, because set_state is now called not only by load_game () but by undo_redo (). Calling get_next_command () can change the history, which we don't want to do while moving around in it.
- Might need a flag that distinguishes these scenarios.

- We should have a list of commands that we pause on, or a function that takes a command and returns true or false. We can also use that list to decide whether to call get_next_command ()? And add_to_history ()? The function would need to take Command_Post_Parse_Type, because Menu and Command are both Command_Post_Parse_Type. Then it would need to match on Command and look at the subtypes/cases.

It's embedded in the commands already? Yes, it's embedded in Command, but Menu and If and End If (along with Command) are in Command_Post_Parse_Type.
Dialogue is a command and has behavior Wait_For_Callback {| continue_afterward = false |}
How do we encode behavior for Menu? In Runner_Run.handle_menu ().
*)
