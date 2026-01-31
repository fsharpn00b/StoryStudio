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

let mutable remove_character_transition_lock = 0

(* Helper functions - interface *)

let private add_to_characters_in_transition
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (character_id : int<character_id>)
    : unit =

    if characters_in_transition.current.Contains character_id then
        do warn "add_to_characters_in_transition" false "Character already in transition." ["character_id", int character_id]
    else do characters_in_transition.current <- characters_in_transition.current.Add character_id

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

    lock (remove_character_transition_lock :> obj) (fun () ->
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

let private fade_in
    (characters : IRefValue<Character_Map>)
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (configuration : Characters_Configuration)
    (character_short_name : string)
    (url : string)
    (position : int<percent>)
    (transition_time : Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "fade_in" String.Empty ["characters", characters; "character_short_name", character_short_name; "url", url; "position", position; "transition_time", transition_time]
    #endif

    match characters.current.TryGetValue character_short_name with
    | true, character ->
        do
            add_to_characters_in_transition characters_in_transition <| character.current.get_id ()
            character.current.fade_in url position transition_time command_queue_item_id
    | _ -> do warn "fade_in" false "Unknown character." ["character_short_name", character_short_name; "characters", characters]

let private fade_out
    (characters : IRefValue<Character_Map>)
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (configuration : Characters_Configuration)
    (character_short_name : string)
    (transition_time : Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "fade_out" String.Empty ["characters", characters; "character_short_name", character_short_name; "transition_time", transition_time]
    #endif

    match characters.current.TryGetValue character_short_name with
    | true, character ->
        do
            add_to_characters_in_transition characters_in_transition <| character.current.get_id ()
            character.current.fade_out transition_time command_queue_item_id
    | _ -> do warn "fade_out" false "Unknown character." ["character_short_name", character_short_name; "characters", characters]

let private cross_fade
    (characters : IRefValue<Character_Map>)
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (configuration : Characters_Configuration)
    (character_short_name : string)
    (url : string)
    (transition_time : Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "cross_fade" String.Empty ["characters", characters; "character_short_name", character_short_name; "url", url; "transition_time", transition_time]
    #endif

    match characters.current.TryGetValue character_short_name with
    | true, character ->
        do
            add_to_characters_in_transition characters_in_transition <| character.current.get_id ()
            character.current.cross_fade url transition_time command_queue_item_id
    | _ -> do warn "cross_fade" false "Unknown character." ["character_short_name", character_short_name; "characters", characters]

let private fade_out_all
    (characters : IRefValue<Character_Map>)
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (transition_time : Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "fade_out_all" String.Empty ["characters", characters; "transition_time", transition_time]
    #endif

    do characters.current |> Seq.iter (fun kv ->
        do
            add_to_characters_in_transition characters_in_transition <| kv.Value.current.get_id ()
            kv.Value.current.fade_out transition_time command_queue_item_id
    )

// TODO1 #transitions Reduce this proliferation of types, functions, parameters, and call layers.
let private move_in
    (characters : IRefValue<Character_Map>)
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (configuration : Characters_Configuration)
    (character_short_name : string)
    (url : string)
    (direction : Character_Move_Direction)
    (position : int<percent>)
    (transition_time : Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "move_in" String.Empty ["characters", characters; "character_short_name", character_short_name; "url", url; "direction", direction; "position", position; "transition_time", transition_time]
    #endif

    match characters.current.TryGetValue character_short_name with
    | true, character ->
        do
            add_to_characters_in_transition characters_in_transition <| character.current.get_id ()
            character.current.move_in url direction position transition_time command_queue_item_id
    | _ -> do warn "move_in" false "Unknown character." ["character_short_name", character_short_name; "characters", characters]

let private move_out
    (characters : IRefValue<Character_Map>)
    (characters_in_transition : IRefValue<int<character_id> Set>)
    (configuration : Characters_Configuration)
    (character_short_name : string)
    (direction : Character_Move_Direction)
    (transition_time : Transition_Time)
    (command_queue_item_id : int<command_queue_item_id>)
    : unit =

    #if debug
    do debug "move_out" String.Empty ["characters", characters; "character_short_name", character_short_name; "url", url; "direction", direction; "position", position; "transition_time", transition_time]
    #endif

    match characters.current.TryGetValue character_short_name with
    | true, character ->
        do
            add_to_characters_in_transition characters_in_transition <| character.current.get_id ()
            character.current.move_out direction transition_time command_queue_item_id
    | _ -> do warn "move_out" false "Unknown character." ["character_short_name", character_short_name; "characters", characters]

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
    let transition_configuration : Transition_Configuration = ()

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
                member _.fade_in
                    (character_short_name : string)
                    (url : string)
                    (position : int<percent>)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    fade_in characters_2 characters_in_transition configuration.current character_short_name url position transition_time command_queue_item_id
                member _.fade_out
                    (character_short_name : string)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    fade_out characters_2 characters_in_transition configuration.current character_short_name transition_time command_queue_item_id
                member _.cross_fade
                    (character_short_name : string)
                    (url : string)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    cross_fade characters_2 characters_in_transition configuration.current character_short_name url transition_time command_queue_item_id                  
                member _.fade_out_all
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    fade_out_all characters_2 characters_in_transition transition_time command_queue_item_id
                member _.move_in
                    (character_short_name : string)
                    (url : string)
                    (direction : Character_Move_Direction)
                    (position : int<percent>)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    move_in characters_2 characters_in_transition configuration.current character_short_name url direction position transition_time command_queue_item_id
                member _.move_out
                    (character_short_name : string)
                    (direction : Character_Move_Direction)
                    (transition_time : Transition_Time)
                    (command_queue_item_id : int<command_queue_item_id>)
                    : unit =
                    move_out characters_2 characters_in_transition configuration.current character_short_name direction transition_time command_queue_item_id
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

    React.fragment <| characters_3
