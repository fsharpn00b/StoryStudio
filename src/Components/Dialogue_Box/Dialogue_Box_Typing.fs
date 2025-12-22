module Dialogue_Box_Typing

// String.Empty
open System

// console, window
open Browser.Dom
open Elmish
open Feliz

open Dialogue_Box_Types
open Dialogue_Box_Transition
open Fade_Types
open Log
open Units_Of_Measure

(* Debug *)

let private log_module_name = "Dialogue_Box_Typing"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1
let mutable private debug_state_update_counter = 1

(* Helper functions *)

let private update_continue
    (notify_transition_complete : int<runner_queue_item_id> -> unit)
    (command_queue_item_id : int<runner_queue_item_id>)
    : Cmd<'a> =

    do notify_transition_complete command_queue_item_id
    Cmd.none

(* Main functions *)

let private reveal_next
    (state_1 : Typing_State)
    (reveal_next_timeout_function_handle : IRefValue<float option>)
    (configuration : IRefValue<Dialogue_Box_Configuration>)
    (command_queue_item_id : int<runner_queue_item_id>)
    : Typing_State * Cmd<Typing_Message> =

    #if debug
    do debug "reveal_next" String.Empty ["state", state_1]
    #endif

    match state_1 with

    | Typing data ->
(* If the user has typing disabled, or if we're finished typing, just change the state to Idle. *)
        if data.typing_speed <= 0<milliseconds> || data.index >= data.text.Length then
            #if debug
            do debug "reveal_next" "Either finished typing, or typing is disabled." ["configuration.typing_speed", dialogue.typing_speed; "index", dialogue.index; "dialogue.text.Length", dialogue.text.Length; "dialogue", dialogue]
            #endif

(* Notify the container (Runner) that the transition is complete so it can run the next command if needed.
We do not need to cancel the transition timeout function because it just dispatches a Reveal_Next message, which is handled by this function. All we have to do is not dispatch that message again. *)
            Idle { character_full_name = data.character; text = data.text }, command_queue_item_id |> Dialogue_Box_Types.Notify_Transition_Complete |> Cmd.ofMsg
        else
            let state_2 = Typing { data with index = data.index + 1; visible_text = data.text.Substring(0, data.index) }
            state_2, Cmd.ofEffect (fun dispatch ->
                do reveal_next_timeout_function_handle.current <- Some <| window.setTimeout ((fun () ->
                    dispatch <| Reveal_Next command_queue_item_id
                ), int data.typing_speed)
            )

(* Otherwise, ignore the call. *)
    | _ ->
        do warn "reveal_next" false "Called with unexpected state. Ignoring." ["state", state_1]
        state_1, command_queue_item_id |> Dialogue_Box_Types.Notify_Transition_Complete |> Cmd.ofMsg

(* The update function we pass to React.useElmish, and the functions it calls, must return a new state and command.

TODO2 We would like to make sure state changes, such as Typing -> Idle, only happen at a single point, such as when we handle Force_Complete_Typing. Unfortunately there is no easy way to do this.
We cannot call dispatch inside update because dispatch is returned by React.useElmish, which itself takes update as a parameter.
We could return Cmd.ofMsg to dispatch a new message, but we must also return the state. Which means we must postpone the state change away from the logic where we determined the state change was needed. It also means an unnecessary additional message dispatch/function call.
As a result, the logic of completing a transition, namely canceling the transition timeout function and notifying the container (Runner) that the transition is complete, is distributed across this file.
x Add a new message, Notify_Complete, that does not change the state or cancel the transition timeout function, but only notifies the container.

- Related. This comment was initially in Fade_Visibility.update_fade_in.
    - We need a better way to handle all this. It is much too easy to have invalid state.
*)
let update_typing_state
    (configuration : IRefValue<Dialogue_Box_Configuration>)
    (reveal_next_timeout_function_handle : IRefValue<float option>)
    (notify_transition_complete : int<runner_queue_item_id> -> unit)
    (message : Typing_Message)
    (state_1 : Typing_State)
    : Typing_State * Cmd<Typing_Message> =

    let debug_data : (string * obj) list = ["state_1", state_1; "message", message :> obj; "reveal_next_timeout_function_handle", reveal_next_timeout_function_handle]

    #if debug
    do
        debug "update" String.Empty <| debug_data @ ["debug_state_update_counter", debug_state_update_counter]
        debug_state_update_counter <- debug_state_update_counter + 1
    #endif

    match message with

    | Begin_Typing data ->
        let state_2 = Typing {
            character = data.character
            text = data.text
            typing_speed = data.typing_speed
            index = 0
            visible_text = String.Empty
            command_queue_item_id = data.command_queue_item_id
        }
        reveal_next state_2 reveal_next_timeout_function_handle configuration data.command_queue_item_id

    | Reveal_Next (command_queue_item_id : int<runner_queue_item_id>) ->
        reveal_next state_1 reveal_next_timeout_function_handle configuration command_queue_item_id

    | Force_Complete_Typing ->
(* If we were typing something, we need to cancel the timeout function that calls reveal_next (). *)
        do cancel_reveal_next_timeout_function configuration reveal_next_timeout_function_handle debug_data

        match state_1 with
        | Typing state_2 ->
            Idle { character_full_name = state_2.character; text = state_2.text }, state_2.command_queue_item_id |> Dialogue_Box_Types.Notify_Transition_Complete |> Cmd.ofMsg

(* Ignore the Empty and Idle states. This should already have been done by Dialogue_Box_Transition.force_complete_transition (), so we issue a warning. *)
        | _ ->
            do warn "update_typing_state" false "Received Force_Complete_Typing message with unexpected state. Ignoring." ["state", state_1]
            state_1, Cmd.none

(* Force_Complete_Typing is for when the player wants to skip a transition.
Set_Dialogue is for when the player wants to load a saved game.
Force_Complete_Typing completes the existing dialogue.
Set_Dialogue discards the existing dialogue and replaces it with the saved dialogue.
Set_Dialogue is dispatched by set_state, which first calls force_complete_transition, which dispatches Force_Complete_Typing.
Force_Complete_Typing is responsible for canceling the transition timeout function and returning the state to Empty or Idle.
We also do not dispatch the Notify_Transition_Complete message here because we must also set the fade state before doing so.
*)
    | Set_Dialogue dialogue ->
        let state_2 = Idle { character_full_name = dialogue.character_full_name; text = dialogue.text }
        match state_1 with
        | Empty -> state_2, Cmd.none
        | Idle _ -> state_2, Cmd.none
(* Force_Complete_Typing should already have set the current state to Empty or Idle. *)
        | Typing dialogue -> error "update_typing_state" "Received Set_Dialogue message with state Typing." ["dialogue", dialogue] |> invalidOp

(* This is dispatched by set_state if the saved dialogue was empty. So, again, any running transition should already have been canceled. *)
    | Set_Empty -> Empty, Cmd.none

    | Dialogue_Box_Types.Notify_Transition_Complete (command_queue_item_id : int<runner_queue_item_id>) -> state_1, update_continue notify_transition_complete command_queue_item_id

(* TODO2 This component has a conflict between between the typing state and the view state.
- Consider whether entering a typing state of Typing or Idle should change the fade state from a non-visible one to a visible one. But what if the player wanted to hide the UI? We should probably leave it to the author to show/hide the dialogue box (for example, when changing scenes).
*)
let type_dialogue
    (state : IRefValue<Typing_State>)
    (configuration : IRefValue<Dialogue_Box_Configuration>)
    (fade_dispatch : Fade_Message<unit> -> unit)
    (typing_dispatch : Typing_Message -> unit)
    (reveal_next_timeout_function_handle : IRefValue<float option>)
    (character : string)
    (text : string)
    (command_queue_item_id : int<runner_queue_item_id>)
    : unit =

    let debug_data : (string * obj) list = ["state", state; "character", character; "text", text; "reveal_next_timeout_function_handle", reveal_next_timeout_function_handle]

    #if debug
    do debug "type_dialogue" String.Empty debug_data
    #endif

(* Make sure the dialogue box is visible.
Our initial typing state is Empty. Dialogue_Box_Rendering.view () renders the Empty state as Html.none. However, our default fade state is Idle_Visible. Once our typing state is non-Empty, view () renders the dialogue box. If we set our fade state to Idle_Hidden, we must change it back by dispatching the Show message.
*)
(* We do not notify the Runner_Queue when we complete the Show transition, because it is an internal method call rather than a command. *)
    fade_dispatch <| Show { data = (); is_notify_transition_complete = false; command_queue_item_id = None }

    match state.current with

(* If the state is Empty, start typing the new text. *)
    | Empty ->
        typing_dispatch <| Begin_Typing {
            character = character
            text = text
            typing_speed = configuration.current.typing_speed
            command_queue_item_id = command_queue_item_id
        }

    | Idle dialogue ->
(* If the state is already Idle and the caller sends us a character and text that are the same as the old, do nothing. *)
        if 0 = String.Compare(character, dialogue.character_full_name) && 0 = String.Compare(text, dialogue.text) then
            do warn "type_dialogue" false "Called with state Idle and same character and text as already displayed. Ignoring." debug_data

(* Otherwise, start typing the new text. *)
        else
            typing_dispatch <| Begin_Typing {
                character = character
                text = text
                typing_speed = configuration.current.typing_speed
                command_queue_item_id = command_queue_item_id
            }

(* Previously, we would cancel the transition timeout function and dispatch Force_Complete_Typing (if the new text was the same as the existing text, meaning the player wanted to skip the transition) or Begin_Typing (if the new text was different than the existing text, meaning the author wanted to abandon typing the existing text and start typing new text). Now we require Runner to call the force_complete_transition () function. That way, we explicitly complete the existing transition and start a new one. *)
    | Typing dialogue ->
        do warn "type_dialogue" false "Called with state Typing. Ignoring." <| debug_data @ ["dialogue", dialogue]
