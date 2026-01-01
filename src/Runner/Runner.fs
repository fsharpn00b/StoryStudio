module Runner

// String.Empty
open System

// IRefValue
open Feliz

open Background
open Character_Types
open Command_Menu
open Command_Types
open Configuration
open Dialogue_Box_Types
open Image_Map
open Log
open Menu
open Music
open Notifications
open Plugins
open Runner_Configuration
open Runner_History
open Runner_Notify
open Runner_Queue
open Runner_Save_Load
open Runner_Types
open Runner_UI
open Save_Load
open Save_Load_Types

// TODO1 Need to see how well this renders on mobile.

(* Notes *)

(* We need to document who is responsible for notifying container and canceling timeout when we interrupt a transition.

A UI component is responsible to manage its own transition timeout function, unless it uses a transition component like Fade.

A UI component is responsible to notify its container (Runner) that a transition is complete. This is because some components (Characters, Dialogue_Box) might handle multiple transitions (multiple characters, fade and typing transitions, respectively). We want the component to send only a single notification for these grouped transitions, because Runner does not concern itself with the individual transitions - those should be contained by the component.

We do not want a component to send the notification more than once, or it will cause Runner to misbehave, because it does not currently distinguish which component is sending the notification, and as a result we might incorrectly signal the completion of a transition for another component (when, in fact, the transition in question has not completed).

1 Letting a transition complete normally
2 Interrupting a transition by running another command

Runner's entry point is Runner_Run.get_next_command ().
That calls force_complete_transitions (), which returns true if no components have transitions in progress. get_next_command () only continues if force_complete_transitions () returns true.
This is case 1.

If any component has a transition in progress, force_complete_transitions () calls the component's force_complete_transition () method with notify_transition_complete set to true.
This is case 2.

Background calls Fade_Transition.force_complete_fade_transition (), passing its dispatch () method.

Characters calls Fade_Transition.force_complete_fade_transition (), passing its dispatch () method, but with notify_transition_complete set to false, for each character.
It then dispatches a Notify_Transition_Complete message itself.
Note that components like Characters, who manage multiple transitions, set notify_transition_complete to false when they complete each transition, so they can issue a single Notify_Transition_Complete message once all transitions are done. This is similar to how Runner_Run.force_complete_transitions () works but on a smaller scale.
This is so components that have fade transitions can notify Runner to run the next command after a transition if they are standalone components, like Background, but not if they are controlled by other components, like characters.
Likewise, certain commands, such as Show and Hide, should notify Runner when they are run by themselves, but not when they are part of a larger operation like loading a saved game.

Dialogue_Box calls Fade_Transition.force_complete_fade_transition (), passing its dispatch () method, but with notify_transition_complete set to false.
It then dispatches a Force_Complete_Typing message, also with notify_transition_complete set to false.
It then dispatches a Notify_Transition_Complete message itself.

Fade_Transition.force_complete_fade_transition () dispatches Show or Hide, as appropriate.
The Show or Hide handler cancels the transition timeout function and dispatches a Notify_Transition_Complete message.

Runner_Run.get_next_command () is called by each component as its transitions complete. get_next_command () does not continue past force_complete_transitions () until all components' transitions are complete.

3 Interrupting a transition by using set_state ()

Runner's entry point is Runner_State.set_state ().
Each component completes its own transitions as part of its set_state () implementation.

Background calls Fade_Transition.force_complete_fade_transition () and dispatches Show or Hide to set the new state, both with notify_transition_complete set to false.
Characters calls Fade_Transition.force_complete_fade_transition () and dispatches Show or Hide to set the new state, both with notify_transition_complete set to false, for each character.
Dialogue_Box calls Fade_Transition.force_complete_fade_transition () and dispatches Force_Complete_Typing, both with notify_transition_complete set to false.
*)

(* Debug *)

let debug_module_name = "Runner"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Component *)

(* Runner needs to be a React component to render the UI components. *)
[<ReactComponent>]
let Runner
    (props : {| expose : IRefValue<I_Runner> |})
    (characters : Character_Input_Map)
    (scenes : Scene_Map)
    : ReactElement =

(* State *)

    let queue = React.useRef <| get_initial_queue ()

(* We let the save/load state live in the Save_Load component, but we keep the configuration state here instead of in the Configuration component. We need the configuration to construct other components, so we must have it available immediately, rather than waiting for the Configuration component to be ready.
TODO2 Would simply constructing the Configuration component first fix this? Still, we could argue the configuration belongs to the Runner rather than to a component whose job is only to show a UI to manage the configuration. This might be a better separation of concerns.
*)
    let configuration_1 = React.useRef (
        match get_configuration_from_local_storage () with
        | Some configuration_2 -> configuration_2
        | None -> default_configuration
    )

(* UI components *)

(* We would prefer to use options for these interfaces rather than null. However, that means we have to unwrap the option every time we want to access <interface>.current. In any case, if any of these interfaces is not available, whether that is represented by null or None, we cannot continue.
TODO2 We could use option with .Value.
*)
    let background_2 = React.useRef<I_Background> Unchecked.defaultof<_>
    let characters_2 = React.useRef<I_Characters> Unchecked.defaultof<_>
    let command_menu_2 = React.useRef<I_Command_Menu> Unchecked.defaultof<_>
    let configuration_component_2 = React.useRef<I_Configuration> Unchecked.defaultof<_>
    let dialogue_box_2 = React.useRef<I_Dialogue_Box> Unchecked.defaultof<_>
    let image_map_2 = React.useRef<I_Image_Map> Unchecked.defaultof<_>
    let menu_2 = React.useRef<I_Menu> Unchecked.defaultof<_>
    let music_2 = React.useRef<I_Music> Unchecked.defaultof<_>
    let notifications_2 = React.useRef<I_Notifications> Unchecked.defaultof<_>
    let save_load_2 = React.useRef<I_Save_Load> Unchecked.defaultof<_>

    let runner_components = React.useRef<Runner_Components> Unchecked.defaultof<_>

(* History is part of state, but must be declared after runner_components. *)
    let history = React.useRef<Runner_History> {
        current_index = None
        history = []
        notify_history_changed = fun () -> runner_components.current.command_menu.current.redraw ()
    }

(*
https://fable-hub.github.io/Feliz/#/Hooks/UseElmish
When you need to trigger events from [an embedded] Elmish component, use React patterns where you pass a callback via the props instead of passing the dispatch function from the parent component.
(end)
*)
    let background_1 =            
(* Pass notify_transition_complete to each UI component so it can notify Runner when it completes a transition. *)
        Background.Background (
            {| expose = background_2 |},
            configuration_1.current.background_configuration,
            (get_notify_transition_complete scenes runner_components queue history Runner_Component_Names.Background)
        )

    let characters_1 = 
        Characters.Characters (
            {| expose = characters_2 |},
            configuration_1.current.characters_configuration,
            (get_notify_transition_complete scenes runner_components queue history Runner_Component_Names.Characters),
            characters
        )

    let command_menu_1 =
        Command_Menu (
            {| expose = command_menu_2 |},
            {
                show_or_hide_configuration_screen = fun () -> Runner_UI.show_or_hide_configuration_screen queue runner_components
                show_save_game_screen = fun () -> Runner_UI.show_saved_game_screen queue runner_components Save_Game
                show_load_game_screen = fun () -> Runner_UI.show_saved_game_screen queue runner_components Load_Game
                can_undo = fun () -> can_undo history
                can_redo = fun () -> can_redo history
                undo = fun () -> Runner_UI.undo queue runner_components history
                redo = fun () -> Runner_UI.redo queue runner_components history
            }
        )

    let configuration_component_1 = Configuration (
        {| expose = configuration_component_2 |},
        configuration_1,
        set_configuration runner_components configuration_1,
(* We must delay this because the Notifications component is not ready yet. *)
        fun () -> notifications_2.current.show_game_paused_notification ()
        )

    let dialogue_box_1 =
        Dialogue_Box.Dialogue_Box (
            {| expose = dialogue_box_2 |},
            configuration_1.current.dialogue_box_configuration,
            (get_notify_transition_complete scenes runner_components queue history Runner_Component_Names.Dialogue_Box)
        )

    let image_map_1 =
        Image_Map.Image_Map (
            {| expose = image_map_2 |},
            (get_notify_transition_complete scenes runner_components queue history Runner_Component_Names.Image_Map),
            (get_notify_image_map_selection scenes queue runner_components)
        )

    let menu_1 =
        Menu.Menu (
            {| expose = menu_2 |},
            (get_notify_transition_complete scenes runner_components queue history Runner_Component_Names.Menu),
            (get_notify_menu_selection scenes queue runner_components)
        )

    let music_1 = Music {| expose = music_2 |}

    let notifications_1 =
        Notifications (
            {| expose = notifications_2 |},
            configuration_1.current.temporary_notifications_configuration
        )

    let save_load_1 =
        Save_Load (
            {| expose = save_load_2 |},
            get_load_game runner_components history queue,
(* As with get_load_game, we must delay this because the Notifications component is not ready yet.
TODO2 It should be, though?
*)
            fun () -> notifications_2.current.show_game_paused_notification ()
        )

(* We cannot call this inside React.useEffectOnce (), as it calls React.useRef (). *)
    let plugins = get_plugins ()
(* This emits the interface for each plugin. Later, we add the component for each plugin to the children for the Runner component. *)
    emit_plugin_interfaces plugins

(* Setup *)

    React.useEffectOnce (fun () ->
        runner_components.current <- {
            background = background_2
            characters = characters_2
            command_menu = command_menu_2
            configuration = configuration_component_2
            dialogue_box = dialogue_box_2
            image_map = image_map_2
            menu = menu_2
            music = music_2
            notifications = notifications_2
            save_load = save_load_2
        }
    )

(* Interface *)

    React.useImperativeHandle(props.expose, fun () ->
        { new I_Runner with
            member _.run (reason : Run_Reason) : unit = Runner_UI.run scenes queue runner_components reason
            member _.show_or_hide_configuration_screen () : unit = Runner_UI.show_or_hide_configuration_screen queue runner_components
// We do not use this for now.
//            member _.hide_configuration_screen (): unit = Runner_UI.hide_configuration_screen runner_components
            member _.handle_escape_key () : unit = Runner_UI.handle_escape_key queue runner_components
            member _.show_saved_game_screen (action : Saved_Game_Action) : unit =
                Runner_UI.show_saved_game_screen queue runner_components action
// We do not use this for now.
//            member _.hide_saved_game_screen () : unit = Runner_UI.hide_saved_game_screen runner_components
            member _.show_or_hide_ui () : unit = Runner_UI.show_or_hide_ui runner_components
            member _.download_screenshot () : unit = download_screenshot_1 ()
            member _.quicksave () : unit = Runner_UI.quicksave queue runner_components
            member _.export_saved_games_from_storage_to_file () : unit = Runner_UI.export_saved_games_from_storage_to_file queue runner_components
            member _.import_saved_games_from_file_to_storage () : unit = Runner_UI.import_saved_games_from_file_to_storage queue runner_components
            member _.export_current_game_to_file () : unit = Runner_UI.export_current_game_to_file queue runner_components
            member _.import_current_game_from_file () : unit = Runner_UI.import_current_game_from_file queue runner_components
            member _.undo () : unit = Runner_UI.undo queue runner_components history
            member _.redo () : unit = Runner_UI.redo queue runner_components history
(* These are for debugging. *)
            member _.show_queue () : unit = do debug "show_queue" String.Empty ["queue", queue.current]
            member _.show_characters () : unit = do characters_2.current.get_character_data ()
            member _.show_background () : unit = do background_2.current.get_background ()
            member _.show_menu_variables () : unit = do debug "show_menu_variables" String.Empty <| ["menu_variables", get_menu_variables queue]
        }
    )


(* Render *)

    let children =
        seq {
            background_1
            characters_1
            command_menu_1
            configuration_component_1
            dialogue_box_1
            image_map_1
            menu_1
            music_1
            notifications_1
            save_load_1
            yield! plugins |> Seq.map (fun kv -> kv.Value.component_)
        }

    React.fragment children
