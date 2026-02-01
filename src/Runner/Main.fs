module Main

// String
open System

// console, window
open Browser.Dom
// Event, KeyboardEvent, WheelEvent
open Browser.Types
open Feliz

// TODO2 Alternate way to apply CSS.
//open Fable.Core.JsInterop
//importSideEffects "./0_pages/menu.css"

open Log
open Parser_2
open Runner
open Runner_Types
open Runner_UI
open Scripts

(* Debug *)

let debug_module_name = "Main"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Set up *)

(* TODO2 Move to separate file like Runner_Set_Up.fs?
- Eventually we might rename this component from Runner_Test to simply Main, since it seems to drive the program at this point.
*)

let character_inputs= get_character_inputs ()
let scenes = get_scene_map_and_javascript (get_scripts ()) (get_backgrounds ()) character_inputs (get_music ())

(* TODO1 #javascript We would need to create the script map here and pass it to I_Runner.run ().

TODO1 #javascript Also, when we check JavaScript, we need to include script names in the output.
*)

(* Component *)

[<ReactComponent>]
let Main () : ReactElement =

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

        window.addEventListener ("keydown", fun (event : Event) -> handle_key_down scenes runner_2 event)
    )

    let runner_1 =
        Runner {| expose = runner_2 |} character_inputs scenes

    runner_1

let root = ReactDOM.createRoot <| document.getElementById "root"
root.render <| Main ()

(* Parser transforms DSL commands into DU commands.
Runner transforms DU commands into UI component function calls.

We cannot translate DSL commands directly to UI component function calls because:
- Some DSL commands, such as conditionals, will not become UI component function calls at all.
- We need to extract information from DU commands for other reasons, such as transition time, for auto-advancing to the next command.

We considered having the author write the entire script in JavaScript. We can emit F# functions so they can be called from JavaScript. However, that has the following problems.
- We need to save the state before and after each command. We could do this by intercepting all F# function calls, but that would be complex. 
- We lose our DSL (for example, laura This is some dialogue.)
*)
