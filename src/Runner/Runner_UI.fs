module Runner_UI

// String
open System

open Browser.Dom
// Event, KeyboardEvent, WheelEvent
open Browser.Types
open Feliz

// TODO2 Alternate way to apply CSS.
//open Fable.Core.JsInterop
//importSideEffects "./0_pages/menu.css"

open Background
open Character_Types
open Dialogue_Box_Types
open Log
open JavaScript_Interop
open JavaScript_Parser
open Parser_2
open Runner
open Runner_Types
open Save_Load_Types
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Parser"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Set up *)

(* TODO2 Move to separate file like Runner_Set_Up.fs?
- Eventually we might rename this component from Runner_Test to simply Main, since it seems to drive the program at this point.
*)

let character_inputs= get_character_inputs ()
let scenes = get_scene_map_and_javascript (get_scripts ()) (get_backgrounds ()) character_inputs

(* Component *)

[<ReactComponent>]
let Runner_Test () : ReactElement =

    let history = React.useRef<Runner_History> {
        current_index = None
        history = []
    }
    let runner_2 = React.useRef<I_Runner> Unchecked.defaultof<_>

    do React.useEffectOnce(fun () ->
        do runner_2.current.run Initial_Run

(* TODO2 Need to write test suite for parser. Which is what this should be. The code in here with the component etc should be in a file called Runner_Test.
- Need to add an exe or test project for testing non-UI-component modules.
*)
        window.addEventListener ("click", fun _ ->
            do runner_2.current.run Player_Run
        )

        window.addEventListener ("wheel", (fun (event_1 : Event) ->
            let event_2 = event_1 :?> WheelEvent

(* TODO2 Make mouse wheel scroll event more responsive. Unfortunately, this seems to be caused by the browser and we cannot find a way to configure it. *)
            #if debug
            do debug "wheel event handler" String.Empty ["Delta Y", event_2.deltaY]
            #endif

            if event_2.deltaY < 0 then do runner_2.current.undo ()
            elif event_2.deltaY > 0 then do runner_2.current.redo ()
        ))

        window.addEventListener ("keydown", fun (event_1 : Event) ->
            let event_2 = event_1 :?> KeyboardEvent
            match event_2.key with
            | "s" -> runner_2.current.show_saved_game_screen Save_Game
            | "l" -> runner_2.current.show_saved_game_screen Load_Game
            | "d" -> runner_2.current.show_saved_game_screen Delete_Game
            | "Escape" ->
                do
                    if not <| runner_2.current.is_configuration_screen_visible () &&
                        not <| runner_2.current.is_saved_game_screen_visible () then
                        runner_2.current.show_configuration_screen ()
                    else
                        runner_2.current.hide_saved_game_screen ()
                        runner_2.current.hide_configuration_screen ()
            | "e" -> runner_2.current.export_saved_games ()
            | "i" -> runner_2.current.import_saved_games ()
            | "c" -> runner_2.current.show_configuration_screen ()
            | "g" -> runner_2.current.download_screenshot ()
            | "u" -> runner_2.current.show_or_hide_ui ()
            | _ -> ()
(* These are for debugging.
TODO1 Also add key handlers for
- h help
- q quicksave
(end)
*)
            match event_2.key with
            | "Q" -> runner_2.current.show_queue ()
            | "C" -> runner_2.current.show_characters ()
            | "B" -> runner_2.current.show_background ()
(* After dumping the JavaScript to the browser console, copy it to a .ts file and run
npx tsc --noEmit --strict <file>
(end)
To install TypeScript, run
npm install typescript --save-dev
(end)
The --save-dev means you are installing the package only for dev purposes, not to be deployed with the application.
*)
            | "J" -> scenes |> check_javascript
            | "T" ->
                show_js_state ()
                runner_2.current.show_menu_variables ()
            | _ -> ()
        )
    )

    let runner_1 =
        Runner {| expose = runner_2 |} history character_inputs scenes

    runner_1

(* Parser transforms DSL commands into DU commands.
Runner transforms DU commands into UI component function calls.

We cannot translate DSL commands directly to UI component function calls because:
- Some DSL commands, such as conditionals, will not become UI component function calls at all.
- We need to extract information from DU commands for other reasons, such as transition time, for auto-advancing to the next command.

We considered having the author write the entire script in JavaScript. We can emit F# functions so they can be called from JavaScript. However, that has the following problems.
- We need to save the state before and after each command. We could do this by intercepting all F# function calls, but that would be complex. 
- We lose our DSL (for example, laura This is some dialogue.)
*)
