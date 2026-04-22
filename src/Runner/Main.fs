module Main

// String
open System

// console, window
open Browser.Dom
// Event, KeyboardEvent, WheelEvent
open Browser.Types
// ? operator
open Fable.Core.JsInterop
open Feliz

// TODO2 Alternate way to apply CSS.
//open Fable.Core.JsInterop
//importSideEffects "./0_pages/menu.css"

open Configuration_Helpers
open Configuration_Types
open Log
open Parser_1_Semantics
open Parser_2_2
open Runner
open Runner_Types_2
open Runner_UI
open Scripts
open Units_Of_Measure

(* Debug *)

let debug_module_name = "Main"

let private debug : log_function = debug debug_module_name
let private warn : warn_function = warn debug_module_name
let private error : error_function = error debug_module_name

(* Set up *)

(* TODO2 Move to separate file like Runner_Set_Up.fs?
- Eventually we might rename this component from Runner_Test to simply Main, since it seems to drive the program at this point.
*)

let database_configuration = get_database_configuration ()
let character_inputs= get_character_inputs ()
(* get_parser uses a try/catch block. *)
let scripts = get_scripts ()
let parser = get_parser scripts (get_backgrounds ()) character_inputs (get_music ())

(* Component *)

[<ReactComponent>]
let Main () : ReactElement =

    let scenes = React.useRef <| get_scene_map parser scripts
    let runner_2 = React.useRef<I_Runner> Unchecked.defaultof<_>

    let configuration_1 = React.useRef (
        match get_configuration_from_local_storage () with
        | Some configuration_2 -> configuration_2
        | None -> default_configuration
    )

    let last_wheel_action_time = ref 0L<milliseconds>

    do React.useEffectOnce(fun () ->
(* 20260422 Code added/changed by AI. *)
(* The runner is temporarily null during fast refresh reloads. Event handlers use this helper to avoid null reference errors. *)
        let with_runner (f : I_Runner -> unit) =
            if isNull (box runner_2.current) then ()
            else f runner_2.current

        do with_runner (fun runner -> runner.run Initial_Run)

        let on_click (_ : Event) =
            do with_runner (fun runner -> runner.run Player_Run)

(* Prevent the player from dragging the viewport. *)
        let on_dragstart (event : Event) =
            event.preventDefault ()

        let on_wheel (event_1 : Event) =
            let event_2 = event_1 :?> WheelEvent

(* TODO2 Make mouse wheel scroll event more responsive. Unfortunately, this seems to be caused by the browser and we cannot find a way to configure it. *)
            #if debug
            do debug "wheel event handler" String.Empty ["Delta Y", event_2.deltaY]
            #endif

            let current_time = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond |> LanguagePrimitives.Int64WithMeasure
            if (current_time - last_wheel_action_time.Value) >= (configuration_1.current.mouse_configuration.wheel_action_elapsed_time_threshold |> int64 |> LanguagePrimitives.Int64WithMeasure) then
                do last_wheel_action_time.Value <- current_time
                if event_2.deltaY < 0 then do with_runner (fun runner -> runner.undo ())
                elif event_2.deltaY > 0 then do with_runner (fun runner -> runner.redo ())

        let on_keydown (event : Event) =
            do with_runner (fun runner -> handle_key_down scenes runner_2 event)

(* Add event listeners using named functions rather than anonymous functions. This lets us remove the event listeners when the component is unmounted. *)
        window.addEventListener ("click", on_click)
        window.addEventListener ("dragstart", on_dragstart)
        window.addEventListener ("wheel", on_wheel)
        window.addEventListener ("keydown", on_keydown)

        React.createDisposable(fun () ->
            window.removeEventListener ("click", on_click)
            window.removeEventListener ("dragstart", on_dragstart)
            window.removeEventListener ("wheel", on_wheel)
            window.removeEventListener ("keydown", on_keydown)
        )
    )

    let runner_1 =
        Runner {| expose = runner_2 |} configuration_1 database_configuration character_inputs scenes parser

    runner_1

(* Create the React root element once and then re-use it across fast refresh reloads. This prevents the warning:
You are calling ReactDOMClient.createRoot() on a container that has already been passed to createRoot() before. Instead, call root.render() on the existing root instead if you want to update it.
(end)
*)
let root =
    if isNull window?__story_studio_root then
        let new_root = ReactDOM.createRoot <| document.getElementById "root"
        do window?__story_studio_root <- new_root
        new_root
    else
        window?__story_studio_root

root?render (Main ())
(* End code added by AI. *)

(* To run, run
npm start
(end)

To build, in root folder, run either:
dotnet fable src --run npx vite build --emptyOutDir
npm run build
(end)
*)

(* Parser transforms DSL commands into DU commands.
Runner transforms DU commands into UI component function calls.

We cannot translate DSL commands directly to UI component function calls because:
- Some DSL commands, such as conditionals, will not become UI component function calls at all.
- We need to extract information from DU commands for other reasons, such as transition time, for auto-advancing to the next command.

We considered having the author write the entire script in JavaScript. We can emit F# functions so they can be called from JavaScript. However, that has the following problems.
- We need to save the state before and after each command. We could do this by intercepting all F# function calls, but that would be complex. 
- We lose our DSL (for example, laura This is some dialogue.)
*)
