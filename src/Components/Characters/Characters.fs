module Characters

// Environment.NewLine
open System

// console, window
open Browser.Dom
// Html, IRefValue, React, ReactComponent, ReactElement
open Feliz

open Character
open Character_Types
open Log
open Transition
open Units_Of_Measure
open Utilities

(* Debug *)

let private log_module_name = "Characters"
let private debug : log_function = debug log_module_name
let private warn : warn_function = warn log_module_name
let private error : error_function = error log_module_name

let mutable debug_render_counter = 1

(* Globa values *)

let mutable characters_in_transition_lock = 0

(* Helper functions - interface *)

(* TODO2 Per AI:
characters_in_transition is keyed only by character_id, not by command_queue_item_id. If overlapping Characters commands ever happen, bookkeeping can collide.
...
Short term: make add/remove symmetry cleaner (same critical section style).
Long term robust fix: track transitions per command id (e.g., Map<command_queue_item_id, Set<character_id>>), then completion checks are isolated per queue item.
(end)

We did the short term fix by adding characters_in_transition_lock to add_to_characters_in_transition ().

We are not sure about the long term fix. We do not expect to allow multiple simultaneous transitions for the same character. Accordingly, we do not allow a character to be added to characters_in_transition if it is already there.

Simultaneous transition () calls for different characters will cause them to be added to characters_in_transition at the same time but for different command_queue_item_ids. So we could change characters_in_transition to use a record type that contains both character_id and command_queue_item_id. 
*)
let private add_to_characters_in_transition
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (character_id : int<character_id>)
    : unit =

    lock (characters_in_transition_lock :> obj) (fun () ->
        if characters_in_transition.current.Contains character_id then
            do warn "add_to_characters_in_transition" false "Character already in transition." ["character_id", int character_id]
        else do characters_in_transition.current <- characters_in_transition.current.Add character_id
    )

let private is_running_transition
    (characters : IRefValue<Character_Map>)
    : bool =
    characters.current |> Seq.exists (fun kv ->
        (kv.Value.current :?> I_Transitionable).is_running_transition ()
    )

let private character_short_name_to_full_name
    (characters : IRefValue<Character_Map>)
    (character_short_name : string)
    : string =
    match characters.current.TryGetValue character_short_name with
    | true, character -> character.current.get_full_name ()
    | _ ->
        error "character_short_name_to_long_name" "Character not found." ["character_short_name", character_short_name; "character short names",  characters.current |> Seq.map (fun kv -> kv.Key) |> Seq.toList :> obj] |> invalidOp

(* Helper functions - state *)

let private force_complete_transition
    (characters : IRefValue<Character_Map>)
    : unit =

(* Only cancel transitions for characters for whom we are running transitions. *)
    do characters.current |> Seq.iter (fun kv -> (kv.Value.current :?> I_Transitionable).force_complete_transition ())

(* Main functions - state *)

let private get_notify_character_transition_complete
    (character_id : int<character_id>)
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (notify_transition_complete : int<command_queue_item_id> -> unit)
    : int<command_queue_item_id> -> unit =

    lock (characters_in_transition_lock :> obj) (fun () ->
        fun (command_queue_item_id : int<command_queue_item_id>) ->
            if characters_in_transition.current.Contains character_id then
                characters_in_transition.current <- characters_in_transition.current.Remove character_id
                if Set.isEmpty characters_in_transition.current then
                    do notify_transition_complete command_queue_item_id
            else
                do warn "get_notify_character_transition_complete" false "Character transition not found." ["character_id", int character_id; "characters_in_transition", characters_in_transition.current |> Set.map int |> Seq.toList :> obj]
    )

let private get_state (characters : IRefValue<Character_Map>) : Characters_Saveable_State =
    characters.current |> Seq.map (fun kv ->
        kv.Key, kv.Value.current.get_state ()
    ) |> Map.ofSeq

let private set_state
    (characters : IRefValue<Character_Map>)
    (state_1 : Characters_Saveable_State)
    : unit =

    do
(* Runner_State.undo_redo () and .show_saved_game_screen () are now responsible for forcing transition completion. *)
//        force_complete_transition characters
        characters.current |> Seq.iter (fun kv ->
(* The list of characters we used to initialize this component and the list of characters in the state parameter must match. *)
            match state_1.TryFind kv.Key with
            | Some character -> kv.Value.current.set_state character
            | None -> error "set_state" "Character not found in state data." ["character", kv.Key; "characters", characters; "state_1", state_1] |> invalidOp
        )

let private set_configuration
    (old_configuration : IRefValue<Characters_Configuration>)
    (new_configuration : Characters_Configuration)
    : unit =
    do old_configuration.current <- new_configuration

(* Main functions - rendering *)

let private transition
    (characters : IRefValue<Character_Map>)
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (configuration : Characters_Configuration)
    (transition_data : Character_Transition_Data)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "transition" String.Empty ["character_short_name", transition_data.character_short_name; "transition_data", transition_data]
    #endif

    match characters.current.TryGetValue transition_data.character_short_name with
    | true, character ->
        do
            add_to_characters_in_transition characters_in_transition <| character.current.get_id ()
            character.current.transition transition_data command_queue_item_id
    | _ -> do warn "transition" false "Unknown character." ["character_short_name", transition_data.character_short_name; "characters", characters]

let private fade_out_all
    (characters : IRefValue<Character_Map>)
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (transition_time : Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "fade_out_all" String.Empty ["transition_time", transition_time]
    #endif

(* Do not fade out hidden characters. *)
    let visible_characters =
        characters.current
            |> Seq.filter (fun kv ->
                match kv.Value.current.get_state () with
                | Hidden -> false
                | _ -> true
            )
(* Convert the sequence to a list so we do not traverse it twice. *)
            |> Seq.toList
(* Finish adding characters to the characters in transition list before we actually start the transitions. get_notify_character_transition_complete () removes characters from the list. If a transition has transition time 0, we could end up removing a character from list before we have finished adding characters to it. *)
    do visible_characters |> List.iter (fun kv ->
        do add_to_characters_in_transition characters_in_transition <| kv.Value.current.get_id ()
    )
    do visible_characters |> List.iter (fun kv ->
        do kv.Value.current.transition { character_short_name = kv.Key; transition = Fade (Fade_Out { transition_time = transition_time }) } command_queue_item_id
    )

(* Component *)

[<ReactComponent>]
let Characters
    (props : {| expose : IRefValue<I_Characters> |},
    characters_configuration : Characters_Configuration,
    notify_transition_complete : int<command_queue_item_id> -> unit,
(* The character list is separate from the configuration because the former is set by the player, and can be updated, whereas the character list is set by the author, and cannot be updated. *)
(* Duplicate character short names should already have been detected by Scripts.get_character_inputs (). *)
    characters_1 : Character_Input_Map)
    : ReactElement =

(* State *)

    let configuration = React.useRef characters_configuration

    let characters_in_transition = React.useRef Set.empty

(* Convert the character input data to components. *)
    let characters_2 = React.useRef Map.empty
    let characters_3 = characters_1 |> Seq.mapi (fun i character_1 ->
        let character_id = i |> LanguagePrimitives.Int32WithMeasure
(* This stores the I_Character interface. See the notes in the Runner constructor. *)
        let character_2 = React.useRef<I_Character> Unchecked.defaultof<_>
(* Create the component based on the input data and expose the interface. *)
        let character_3 = Character (
            {| expose = character_2 |},
            character_1.Value,
            character_id,
            get_notify_character_transition_complete character_id characters_in_transition notify_transition_complete
        )
(* Add the I_Character interface to the map. The key is the character's short name. *)
        do characters_2.current <- characters_2.current.Add (character_1.Key, character_2)
(* Return the component to be rendered. *)
        character_3
    )

(* Interface *)

    do React.useImperativeHandle(props.expose, fun () ->
        {
            new I_Characters with

                member _.transition
                    (transition_data : Character_Transition_Data)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =

                    transition characters_2 characters_in_transition configuration.current transition_data command_queue_item_id
              
                member _.fade_out_all
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    fade_out_all characters_2 characters_in_transition transition_time command_queue_item_id

                member _.get_state () : Characters_Saveable_State = get_state characters_2
                member _.set_state (state : Characters_Saveable_State) : unit = set_state characters_2 state
                member _.set_configuration (new_configuration : Characters_Configuration) = set_configuration configuration new_configuration
                member _.get_configuration () : Characters_Configuration = configuration.current
                member _.character_short_name_to_full_name (character_short_name : string) : string = character_short_name_to_full_name characters_2 character_short_name
(* This is for debugging. *)
                member _.get_character_data () : unit =
                    do debug "get_character_data" String.Empty ["characters", characters_2.current |> Seq.map (fun kv -> kv.Value.current.get_character_data ()) |> Seq.toList :> obj]

            interface I_Transitionable with
                member _.is_running_transition () : bool = is_running_transition characters_2
                member _.force_complete_transition (): unit = force_complete_transition characters_2
                member _.get_name () : string = "Characters"
        }
    )

(* Render *)

    Html.div [
(* For now there is no such class, but this does not cause an error. *)
        prop.id "characters_container"
        prop.className "interface_layer"
(* TODO1 #characters If two characters occupy the same position, we will need to add individual character z-indices, which the author must set when they display each character. z-index would be an optional parameter to fadein, movein, etc. It would start from 1 and we would add it to the base characters_container_z_index. We would also need to increment the next z-index (dialogue_box_z_index) to 10 or so to make room for the character z-indices. Unless z-indices within the character container are already relative (i.e. if the character container has z-index 1 and the next sibling UI element has z-index 2, maybe a character can have a z-index of 3 and still be below the dialogue box).
*)
        prop.style [style.zIndex characters_container_z_index]
        prop.children characters_3
    ]
