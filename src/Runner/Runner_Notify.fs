module Runner_Notify

// console, window
open Browser.Dom
open Feliz

open Command_Types
open Log
open Runner_Queue
open Runner_Queue_Transition
open Runner_Types
open Units_Of_Measure
open Utilities

(* Debug *)

let debug_module_name = "Runner_Notify"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Main functions *)

(* TODO2 We have a single get_notify_transition_complete function. Is it possible to have separate ones for fade and typing? We trigger both from Dialogue_Box.force_complete_transition (). Yet we can't change the command state is_running true -> false for each transition completion notification (fade and typing). That would be a collision.
For now we have to assume all transitions for a given component are simultaneous, or at least can be ordered to complete simultaneously. For our purposes, all we want is to be able to continue to the next command.
*)
(* TODO2 Can we replace this function by having Runner provide each component with a dispatch method that it can use to send Notify_Transition_Complete, rather than making a function that is closed over all these values?
But then we would need a Runner.update () function. It would essentially be a rename of this function. It would add an extra step, though it might be more idiomatic. We would still need the fun () -> delay to populate runner_component_interfaces.
In Runner:
let state, dispatch = React.useElmish((<state>, Cmd.none), update scenes runner_component_interfaces_1 history command_state, [||])
The Notify_Transition_Complete message would include the sender.
- What would the state be? command_state?

- We still need this function if only to delay the call to get_next_command () until the UI components are available.
*)
(* A UI component uses this function to notify Runner that the component has finished its current transition. *)
let get_notify_transition_complete
    (scenes : IRefValue<Scene_Map>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (queue : IRefValue<Runner_Queue>)
    (history : IRefValue<Runner_History>)
    (component_id : Runner_Component_Names)
    (parser : Parser)
    : int<command_queue_item_id> -> unit =
(* Close over all these parameters so it becomes a unit -> unit that we can pass to UI components.
We also need to delay the evaluation of this function until runner_component_interfaces is not null. The delayed result of this function is passed to the constructors of these components.
We can close over queue because it is a reference.
*)
    fun (command_queue_item_id : int<command_queue_item_id>) ->
(* If we do not delay here, and a transition is short enough, this notification can set the queue state to Queue_Idle *before* the transition sets it to Queue_Running. As a result, it is not possible to start any more transitions. *)
        do window.setTimeout ((fun () ->
            do remove_transition_1 queue history scenes runner_component_interfaces command_queue_item_id component_id parser
        ), int notify_transition_complete_delay_time) |> ignore

(* notify_transition_complete is only called when a transition completes on its own, without being interrupted. *)
(* We still must delay here in case the current command shows a menu. Otherwise, when we call Menu.get_state (), the menu state is still Hidden even though a message has been dispatched to show the menu, and we end up incorrectly saving the outdated value.

See notes in Fade_Transition.update_complete_transition (), Fade_Visibility.update_hide (), and Menu.update ().
*)

let get_notify_menu_selection
    (scenes : IRefValue<Scene_Map>)
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (parser : Parser)
    : string -> int -> unit =

    fun (menu_name : string) (selected_index : int) ->
        do
(* As of 20251209 we keep a map of all menus the player has seen and their corresponding choices. When we evaluate any JavaScript statement or conditional, we inject all menu variables into the browser's JavaScript environment beforehand. We must do this because we evaluate each JavaScript statement/conditional separately, and there is no way to maintain a JavaScript environment from one evaluation to the next, except by attaching variables to the window object.

This means we must inject menu variables to evaluate the following.
x inline JS
x block JS
x dialogue (string)
x if (bool)
x else if (bool)
x menu descriptions (string)
x menu item descriptions (string)
x menu item availability (bool)
(end)

Previously (20251208) we kept menu and choice data in window.state.menus. The trouble was:
1 The author had to declare every menu name beforehand in the TypeScript type file.
2 Because each menu variable was declared beforehand as an object field, there was no way to tell if the author accessed it before the menu was shown.

See also notes in Runner_Types.Command_Queue_State_Idle_Data.
(end)
*)
            match queue.current with
            | Queue_Idle data ->
                queue.current <- Queue_Idle { data with menu_variables = data.menu_variables.Add (menu_name, selected_index) }
            | _ -> error "get_notify_menu_selection" "Unexpected queue state." ["Queue state", queue.current] |> invalidOp

(* When the player selects a menu item, it also registers as a mouse click, which Runner_Test handles by calling Runner.run (). In Runner.run (), we do not call get_next_command () if a menu is visible. We want to make sure the menu is still visible when Runner.run () is called, so we delay before we call Menu.hide ().

Previously, we called menu.hide () without a delay. We did not need to call Runner_Run.get_next_command (). We think that worked because the menu was hidden before the player's mouse click event to select a menu item was registered. That mouse click event also called get_next_command (), and by then the menu was hidden, so get_next_command () continued past force_complete_transitions (). Because that essentially relies on a fluke of timing, we worry it is not reliable.
*)
            window.setTimeout((fun () ->
(* We do not have Menu.hide () call get_notify_transition_complete () because this is an internal command, as opposed to one run by the player.

The command behavior for Menu is Wait_For_Callback/continue_after_finished = false, and that is still the current command, so Runner would not call run () anyway. Instead, we call run () here.

That is because the Menu command is for showing the menu, and after that, the command and its transition are complete. It does not concern itself with menu item selection. In effect, menu item selection is not a command and does not have an associated behavior.
*)
// TODO2 Can this be done in the Menu.Menu_Item_Selected handler? Just return false for is_visible and clear menu data there. No, it might be we still want to make sure no mouse clicks are processed yet.
                runner_component_interfaces.current.menu.current.hide false None
                window.setTimeout((fun () ->
                    run queue scenes runner_component_interfaces Run_Reason.Notify_Menu_Selection parser
                ), int notify_transition_complete_delay_time) |> ignore
            ), int notify_transition_complete_delay_time) |> ignore

let get_notify_image_map_selection
    (scenes : IRefValue<Scene_Map>)
    (queue : IRefValue<Runner_Queue>)
    (runner_component_interfaces : IRefValue<Runner_Component_Interfaces>)
    (parser : Parser)
    : string -> int -> unit =

    fun (image_map_name : string) (selected_index : int) ->
        do
            match queue.current with
            | Queue_Idle data ->
(* Save image map variables in the same map as menu variables. *)
                queue.current <- Queue_Idle { data with menu_variables = data.menu_variables.Add (image_map_name, selected_index) }
            | _ -> error "get_notify_image_map_selection" "Unexpected queue state." ["Queue state", queue.current] |> invalidOp

            window.setTimeout((fun () ->
                window.setTimeout((fun () ->
                    run queue scenes runner_component_interfaces Run_Reason.Notify_Image_Map_Selection parser
                ), int notify_transition_complete_delay_time) |> ignore
            ), int notify_transition_complete_delay_time) |> ignore

