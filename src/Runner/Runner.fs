module Runner

// String.Empty
open System

// console
open Browser.Dom
open Feliz

open Background
open Character_Types
open Command_Types
open Configuration
open Dialogue_Box_Types
open Log
open Menu
open Music
open Runner_Helpers
open Runner_Queue
open Runner_State
open Runner_Transition
open Runner_Types
open Save_Load
open Save_Load_Types
open Units_Of_Measure
open Utilities

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

(* Configuration *)

let private default_background_configuration : Background_Configuration = {
    placeholder = ()
}
let private default_characters_configuration : Characters_Configuration = {
    placeholder = ()
}
let private default_dialogue_box_configuration : Dialogue_Box_Configuration = {
    typing_speed = 0<milliseconds>
}

let private default_configuration = {
    background_configuration = default_background_configuration
    characters_configuration = default_characters_configuration
    dialogue_box_configuration = default_dialogue_box_configuration
}

(* Component *)

(* Runner needs to be a React component to render the UI components. *)
[<ReactComponent>]
let Runner
    (props : {| expose : IRefValue<I_Runner> |})
    (history : IRefValue<Runner_History>)
    (characters : Character_Input_Map)
    (scenes : Scene_Map)
    : ReactElement =

(* State *)

    let queue = React.useRef <| get_initial_queue ()

(* UI components *)

(* We would prefer to use options for these interfaces rather than null. However, that means we have to unwrap the option every time we want to access <interface>.current. In any case, if any of these interfaces is not available, whether that is represented by null or None, we cannot continue.
TODO2 We could use option with .Value.
*)
    let background_2 = React.useRef<I_Background> Unchecked.defaultof<_>
    let dialogue_box_2 = React.useRef<I_Dialogue_Box> Unchecked.defaultof<_>
    let characters_2 = React.useRef<I_Characters> Unchecked.defaultof<_>
    let menu_2 = React.useRef<I_Menu> Unchecked.defaultof<_>
    let save_load_2 = React.useRef<I_Save_Load> Unchecked.defaultof<_>
    let music_2 = React.useRef<I_Music> Unchecked.defaultof<_>
    let configuration_component_2 = React.useRef<I_Configuration> Unchecked.defaultof<_>
    let runner_components = React.useRef<Runner_Components> Unchecked.defaultof<_>

(* We let the save/load state live in the Save_Load component, but we keep the configuration state here instead of in the Configuration component. We need the configuration to construct other components, so we must have it available immediately, rather than waiting for the Configuration component to be ready.
TODO2 Would simply constructing the Configuration component first fix this? Still, we could argue the configuration belongs to the Runner rather than to a component whose job is only to show a UI to manage the configuration. This might be a better separation of concerns.
*)
    let configuration_1 = React.useRef (
        match get_configuration_from_local_storage () with
        | Some configuration_2 -> configuration_2
        | None -> default_configuration
    )

(* TODO1 Add music.
- Add fade in/fade out/cross fade.

- Need to save music as part of saved game state and play it on load.
- Music stops if we refresh page.
*)
// TODO1 Need to see how well this renders on mobile.

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

    let dialogue_box_1 =
        Dialogue_Box.Dialogue_Box (
            {| expose = dialogue_box_2 |},
            configuration_1.current.dialogue_box_configuration,
            (get_notify_transition_complete scenes runner_components queue history Runner_Component_Names.Dialogue_Box)
        )

    let characters_1 = 
        Characters.Characters (
            {| expose = characters_2 |},
            configuration_1.current.characters_configuration,
            (get_notify_transition_complete scenes runner_components queue history Runner_Component_Names.Characters),
            characters
        )

    let menu_1 =
        Menu.Menu (
            {| expose = menu_2 |},
            (get_notify_transition_complete scenes runner_components queue history Runner_Component_Names.Menu),
            (get_notify_menu_selection scenes queue runner_components)
        )

    let save_load_1 =
        Save_Load (
            {| expose = save_load_2 |},
            get_load_game runner_components history queue 
        )

    let music_1 = Music {| expose = music_2 |}

    let configuration_component_1 = Configuration ({| expose = configuration_component_2 |}, configuration_1, set_configuration runner_components configuration_1)

    React.useEffectOnce(fun () ->
        runner_components.current <- {
            background = background_2
            characters = characters_2
            dialogue_box = dialogue_box_2
            menu = menu_2
            save_load = save_load_2
            music = music_2
            configuration = configuration_component_2
        }
    )

    let children =
        seq {
            background_1
            dialogue_box_1
            characters_1
            menu_1
            save_load_1
            music_1
            configuration_component_1
        }

(*
TODO2 We could separate out
1 Hosting the individual components.
2 Converting the DUs to component calls. However, that adds another interface layer between 1 and 2.
3 Actually running the calls.

This is less urgent now that we've split up Runner.fs.
*)

(* Interface *)

    React.useImperativeHandle(props.expose, fun () ->
        { new I_Runner with
            member _.run (reason : Run_Reason) : unit =
(* When a menu or the save/load screen or configuration screen is visible, we do not want to run the next command. We could simply ignore mouse clicks except for those handled by the menu or save/load screen or configuration, but this is safer, in case this method is called for some other reason. *)
                if not <| runner_components.current.menu.current.is_visible () &&
                    not <| runner_components.current.save_load.current.is_visible () &&
                    not <| runner_components.current.configuration.current.is_visible () then
// TODO1 After we save/load screen, show an overlay that says game paused; press click to continue. What about setting that in the dialogue box?
(* We must determine what the next command is before we can run it. *)
                    run queue scenes runner_components reason
(* We do not use these for now. *)
(*
            member _.get_state () : Runner_Saveable_State =
                get_state runner_components queue
            member _.set_state (state : Runner_Saveable_State) =
                set_state state queue runner_components
*)
            member _.show_configuration_screen () : unit =
                show_configuration_screen queue runner_components
            member _.hide_configuration_screen (): unit =
                runner_components.current.configuration.current.hide ()
            member _.handle_escape_key () : unit =
                do
                    if not <| runner_components.current.configuration.current.is_visible () &&
                        not <| runner_components.current.save_load.current.is_visible () then
                        runner_components.current.configuration.current.show ()
                    else
                        runner_components.current.save_load.current.hide ()
                        runner_components.current.configuration.current.hide ()
            member _.show_saved_game_screen (action : Saved_Game_Action) : unit =
                show_saved_game_screen queue runner_components action
            member _.hide_saved_game_screen () : unit =
                hide_saved_game_screen runner_components
            member _.show_or_hide_ui () : unit =
                show_or_hide_ui runner_components
            member _.download_screenshot () : unit =
                download_screenshot_1 ()
            member _.quicksave () : unit =
                quicksave_or_autosave queue runner_components Quicksave
            member _.export_saved_games_from_storage_to_file () : unit = 
                runner_components.current.save_load.current.export_saved_games_from_storage_to_file ()
            member _.import_saved_games_from_file_to_storage () : unit =
                runner_components.current.save_load.current.import_saved_games_from_file_to_storage ()
            member _.export_current_game_to_file () : unit =
                export_current_game_to_file queue runner_components
            member _.import_current_game_from_file () : unit =
                runner_components.current.save_load.current.import_current_game_from_file ()
            member _.undo () : unit =
(* When the save/load or configuration screen is visible, we do not want to undo or redo. *)
                if not <| runner_components.current.save_load.current.is_visible () &&
                    not <| runner_components.current.configuration.current.is_visible () then
                    undo_redo history queue runner_components true
            member _.redo () : unit =
(* When the save/load screen is visible, we do not want to undo or redo. *)
                if not <| runner_components.current.save_load.current.is_visible () &&
                    not <| runner_components.current.configuration.current.is_visible () then
                    undo_redo history queue runner_components false
(* These are for debugging. *)
            member _.show_queue () : unit = do debug "show_queue" String.Empty ["queue", queue.current]
            member _.show_characters () : unit = do characters_2.current.get_character_data ()
            member _.show_background () : unit = do background_2.current.get_background ()
            member _.show_menu_variables () : unit = do debug "show_menu_variables" String.Empty <| ["menu_variables", get_menu_variables queue]
        }
    )

(* Render *)

    React.fragment children
