module Dialogue_Box_State

// IRefValue
open Feliz

open Dialogue_Box_Types
open Log
open Transition_Types

(* Debug *)

let private log_module_name = "Dialogue_Box_State"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable private debug_render_counter = 1
let mutable private debug_state_update_counter = 1


(* Main functions - state *)

let is_visible
    (fade_state : IRefValue<Transition_State<Dialogue_Box_Visibility_State, Dialogue_Box_Transition_Type>>)
    : bool =
    match fade_state.current with
    | Idle Hidden -> false
    | Idle Visible -> true
    | _ -> error "is_visible" "Unexpected Fade_State. Ignoring." ["fade_state", fade_state] |> invalidOp

let get_state
    (typing_state : IRefValue<Typing_State>)
    (fade_state : IRefValue<Transition_State<Dialogue_Box_Visibility_State, Dialogue_Box_Transition_Type>>)
    : Dialogue_Box_Saveable_State =

    let dialogue =
        match typing_state.current with
        | Empty -> None
        | Typing_State.Idle dialogue -> Some dialogue
        | Typing dialogue ->
            Some {
                character_full_name = dialogue.character
                text = dialogue.text
            }
(* The visible field represents only the fade state. If the typing state is Empty, we already represent that with dialogue = None. view () hides the dialogue box if the typing state is Empty, but if we did that here, it would be a loss of information. For example if the fade state was Idle_Visible, but the typing state was Empty, and we set visible = false because of the latter, then when we loaded the state, the fade state would be incorrectly set to Idle_Hidden.
*)
    {
        dialogue = dialogue
        visible =
            match fade_state.current with
            | Idle Hidden -> Hidden
            | Idle Visible -> Visible
            | _ -> error "get_state" "Unexpected Fade_State. Ignoring." ["fade_state", fade_state] |> invalidOp
    }

let set_state
    (typing_dispatch : Typing_Message -> unit)
    (fade_dispatch : Transition_Message<Dialogue_Box_Visibility_State, Dialogue_Box_Transition_Type> -> unit)
    (saved_state : Dialogue_Box_Saveable_State)
    : unit =
    do
(* Runner_State.undo_redo () and .show_saved_game_screen () are now responsible for forcing transition completion. *)
//        force_complete_transition current_fade_state current_typing_state fade_dispatch typing_dispatch

        fade_dispatch <|
            match saved_state.visible with
            | Visible -> Skip_Transition { transition_type = Fade; new_data = Visible; is_notify_transition_complete = false; command_queue_item_id = None }
            | Hidden -> Skip_Transition { transition_type = Fade; new_data = Hidden; is_notify_transition_complete = false; command_queue_item_id = None }
(* Set_Dialogue and Set_Empty do not dispatch Notify_Transition_Complete. See notes in Dialogue_Box_Typing.update_typing_state. *)
        typing_dispatch <| match saved_state.dialogue with | Some dialogue -> Set_Dialogue dialogue | None -> Set_Empty

let set_configuration
    (old_configuration : IRefValue<Dialogue_Box_Configuration>)
    (new_configuration : Dialogue_Box_Configuration)
    : unit =
    do old_configuration.current <- new_configuration
