module Dialogue_Box

// console, window
open Browser.Dom
// Cmd
open Elmish
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz
// useElmish
open Feliz.UseElmish

open Dialogue_Box_Rendering
open Dialogue_Box_State
open Dialogue_Box_Transition
open Dialogue_Box_Types
open Dialogue_Box_Typing
open Log
open Transition
open Transition_Types
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Dialogue_Box"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1
let mutable private debug_state_update_counter = 1

(* TODO2 How do you test this? How do you separate logic from UI to make a test suite such that a certain sequence of messages produces a certain result (state)? *)

(* TODO2 Should we have a message supertype for both fade and typing messages and a single update function to handle both?
Typing state and fade state each need a separate update function to pass to useElmish, but maybe it can be the same function which then breaks down the combined type with pattern matching.
On the other hand, a given component should only expect to have to process one message at a time anyway?
*)

(* Component *)

[<ReactComponent>]
let Dialogue_Box (
    props : {| expose : IRefValue<I_Dialogue_Box> |},
    configuration : Dialogue_Box_Configuration,
    notify_transition_complete : int<runner_queue_item_id> -> unit)
    : ReactElement =

(* State *)

    let configuration = React.useRef configuration

(* We need to record the ID of the timeout function created for reveal_next() so we can cancel it if needed. 
If we don't do this, when we change the dialogue text before it finishes typing, the typing speed increases, which suggests the old instance of reveal_next() is still running.
*)
(* We could make reveal_next_timeout_function_handle part of Typing_Dialogue.
However, previously, when we were using React, we made fade_transition_timeout_function_handle part of Fade In/Fade_Out/Cross_Fade Transition_State_Data. That caused an infinite loop of state updates and re-renders.
That should not apply here, since updates should only be triggered by dispatched messages, but we feel it is better not to take chances.
*)
    let reveal_next_timeout_function_handle = React.useRef None

(* As far as we can tell, calls to React.use* are essentially pinned. That is, they run when the component render function first runs, but not again after that.

To make them run again in response to a change to a value, add the value to the dependencies array. See
https://github.com/fable-hub/Feliz/blob/master/public/Feliz.UseElmish/Index.md#understading-the-dependencies-array
(end)

In this case, we do not need any dependencies. We can just run update whenever a message is dispatched.
*)
    let typing_state, typing_dispatch = React.useElmish(init = (Empty, Cmd.none), update = update_typing_state configuration reveal_next_timeout_function_handle notify_transition_complete, dependencies = [||])
    let typing_state_ref = React.useRef typing_state
    do typing_state_ref.current <- typing_state

    let fade_transition_timeout_function_handle = React.useRef None
    let fade_configuration = ()
(* We set the fade state to Idle_Visible at the start, but in the view function, that is overridden by the typing state of Empty. *)
    let fade_state, fade_dispatch = React.useElmish((Idle Visible, Cmd.none), Transition.update fade_configuration fade_transition_timeout_function_handle notify_transition_complete, [||])
    let fade_state_ref = React.useRef fade_state
    do fade_state_ref.current <- fade_state

(* Interface *)

    do React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Dialogue_Box with
                member _.show
                    (is_notify_transition_complete : bool)
                    (command_queue_item_id : int<runner_queue_item_id> option)
                    : unit =
                        fade_dispatch <| Skip_Transition {
                            transition_type = Fade
                            new_data = Visible
                            is_notify_transition_complete = is_notify_transition_complete
                            command_queue_item_id = command_queue_item_id
                        }
                member _.hide
                    (is_notify_transition_complete : bool)
                    (command_queue_item_id : int<runner_queue_item_id> option)
                    : unit =
                        fade_dispatch <| Skip_Transition {
                            transition_type = Fade
                            new_data = Hidden
                            is_notify_transition_complete = is_notify_transition_complete
                            command_queue_item_id = command_queue_item_id
                        }
                member _.is_visible () : bool = is_visible fade_state_ref
                member _.type_dialogue (character : string) (text : string) (command_queue_item_id : int<runner_queue_item_id>) = type_dialogue typing_state_ref configuration fade_dispatch typing_dispatch reveal_next_timeout_function_handle character text command_queue_item_id
                member _.set_configuration (new_configuration : Dialogue_Box_Configuration) = set_configuration configuration new_configuration
                member _.get_configuration () : Dialogue_Box_Configuration = configuration.current
                member _.get_state () = get_state typing_state_ref fade_state_ref
                member _.set_state (state : Dialogue_Box_Saveable_State) = set_state typing_dispatch fade_dispatch state
            interface I_Transitionable with
                member _.is_running_transition (): bool = Dialogue_Box_Transition.is_running_transition fade_state_ref typing_state_ref
                member _.force_complete_transition () : unit = Dialogue_Box_Transition.force_complete_transition fade_state_ref typing_state_ref fade_dispatch typing_dispatch
                member _.get_name () : string = "Dialogue Box"
        }
    )

(* Render *)

    view typing_state_ref fade_state_ref configuration.current
