module Parser_Command_Data

open Command_Types
open Parser_Patterns

(* Consts *)

let private command_patterns_1 = [
    {
        name = CN_Music_Play
        pattern = "playmusic"
        parameters = [
            {
                name = "name"
                description = "The name of the music track. Music track names are linked to URLs in the music configuration file."
                type' = Word
            }
        ]
    }
    {
        name = CN_Music_Stop
        pattern = "stopmusic"
        parameters = []
    }
    {
        name = CN_Background_Fade_In
(* TODO1 #parsing Make this field a list of aliases and match on any of them. This way, we can have shorter aliases like fi, fo, xf, etc.
*)
        pattern = "fadein"
        parameters = [
            {
                name = "name"
                description = "The name of the background image. Background names are linked to URLs in the background configuration file."
                type' = Word
            }
            {
                name = "transition_time"
                description = "Transition time (seconds)."
                type' = Float
            }
        ]
    }
    {
        name = CN_Background_Fade_In
        pattern = "fadeout"
        parameters = [
            {
                name = "transition_time"
                description = "Transition time (seconds)."
                type' = Float
            }
        ]
    }
    {
        name = CN_Background_Fade_Out
        pattern = "fadeto"
        parameters = [
            {
                name = "name"
                description = "The name of the background image. Background names are linked to URLs in the background configuration file."
                type' = Word
            }
            {
                name = "transition_time"
                description = "Transition time (seconds)."
                type' = Float
            }
        ]
    }
    {
        name = CN_Character_Fade_In
        pattern = "fadein"
        parameters = [
            {
                name = "name"
                description = "The character name. Character names are defined in the character configuration file."
                type' = Word
            }
            {
                name = "sprite"
                description = "The character sprite name. Character sprite names are linked to URLs in the character configuration file."
                type' = Word
            }
            {
                name = "position"
                description = "The character position (percent of screen width from left)."
                type' = Int
            }
            {
                name = "transition_time"
                description = "Transition time (seconds)."
                type' = Float
            }
        ]
    }
    {
        name = CN_Character_Fade_Out
        pattern = "fadeout"
        parameters = [
            {
                name = "name"
                description = "The character name. Character names are defined in the character configuration file."
                type' = Word
            }
            {
                name = "transition_time"
                description = "Transition time (seconds)."
                type' = Float
            }
        ]
    }
    {
        name = CN_Character_Cross_Fade
        pattern = "fadeto"
        parameters = [
            {
                name = "name"
                description = "The character name. Character names are defined in the character configuration file."
                type' = Word
            }
            {
                name = "sprite"
                description = "The character sprite name. Character sprite names are linked to URLs in the character configuration file."
                type' = Word
            }
            {
                name = "transition_time"
                description = "Transition time (seconds)."
                type' = Float
            }
        ]
    }
    {
        name = CN_Fade_Out_All
        pattern = "fadeoutall"
        parameters = [
            {
                name = "transition_time"
                description = "Transition time (seconds)."
                type' = Float
            }
        ]
    }
    {
        name = CN_Dialogue_Box_Show
        pattern = "showdialoguebox"
        parameters = []
    }
    {
        name = CN_Dialogue_Box_Hide
        pattern = "hidedialoguebox"
        parameters = []
    }
    {
        name = CN_Temporary_Notification
        pattern = "notify"
        parameters = [
            {
                name = "text"
                description = "The notification text."
                type' = Any_Min_Length_1
            }
        ]
    }
    {
        name = CN_Permanent_Notification
        pattern = "status"
        parameters = [
            {
                name = "text"
                description = "The notification text."
                type' = Any_Min_Length_1
            }
        ]
    }
    {
        name = CN_JavaScript_Inline
        pattern = "js"
        parameters = [
            {
                name = "code"
                description = "The JavaScript code."
                type' = Any_Min_Length_1
            }
        ]
    }
    {
        name = CN_If
        pattern = "if"
        parameters = [
            {
                name = "conditional"
                description = "The conditional."
                type' = Any_Min_Length_1
            }
        ]
    }
    {
        name = CN_Else_If
        pattern = "elseif"
        parameters = [
            {
                name = "conditional"
                description = "The conditional."
                type' = Any_Min_Length_1
            }
        ]
    }
    {
        name = CN_Else
        pattern = "else"
        parameters = []
    }
    {
        name = CN_End_If
        pattern = "endif"
        parameters = []
    }
    {
        name = CN_Jump
        pattern = "jump"
        parameters = [
            {
                name = "destination"
                description = "The jump destination."
                type' = Word
            }
        ]
    }
    {
        name = CN_Menu
        pattern = "menu"
        parameters = [
            {
                name = "name"
                description = "The menu name."
                type' = Word
            }
            {
                name = "description"
                description = "The menu description."
                type' = Any_Min_Length_1
            }
        ]
    }
    {
        name = CN_End_Menu
        pattern = "endmenu"
        parameters = []
    }
    {
        name = CN_Image_Map
        pattern = "imagemap"
        parameters = [
            {
                name = "name"
                description = "The image menu name."
                type' = Word
            }
            {
                name = "background"
                description = "The name of the background image."
                type' = Word
            }
            {
                name = "transition_time"
                description = "The transition time (seconds)."
                type' = Float
            }
        ]
    }
    {
        name = CN_End_Image_Map
        pattern = "endimagemap"
        parameters = [
            {
                name = "transition_time"
                description = "The transition time (seconds)."
                type' = Float
            }
        ]
    }
]

(* Functions *)

let private command_parameter_type_to_pattern (type' : Command_Parameter_Type) : string =
    match type' with
    | Any_Min_Length_0 -> any_min_length_0_pattern
    | Any_Min_Length_1 -> any_min_length_1_pattern
    | Word -> word_plus_dash_pattern
    | Int -> int_pattern
    | Float -> float_pattern
(* We do not use this for now. *)
//    | Other x -> x.pattern

let command_patterns_2 =
    command_patterns_1
        |> List.map (fun pattern ->
            {
                name = pattern.name
                pattern = pattern.pattern
                parameters =
                    pattern.parameters
                        |> List.map (fun parameter -> parameter.name, parameter.type')
                        |> Map.ofList
                parameters_regex =
                    if List.isEmpty pattern.parameters then None
                    else
                        let pattern =
                            pattern.parameters
                                |> List.map (fun parameter ->
                                    $"(?<{parameter.name}>{parameter.type' |> command_parameter_type_to_pattern})"
                                )
                                |> List.reduce (fun acc item -> $@"{acc}\s+{item}")
                        $"^{pattern}$"
                            |> get_compiled_regex
                            |> Some
            }
        )
