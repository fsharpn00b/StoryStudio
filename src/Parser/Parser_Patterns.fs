module Parser_Patterns

open System.Text.RegularExpressions

(* This is a helper function, but is used to define consts, and is almost a const itself due to the regex configuration options. *)
let get_compiled_regex (pattern : string) = Regex (pattern, RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

let any_min_length_0_pattern = @".*"
let any_min_length_1_pattern = @".+"

(* \w includes _, but not -. *)
let word_plus_dash_pattern = @"[\w-]+"

let int_pattern = @"\d+"
(* TODO2 This will also match an int. So if we want to try to match either a float or int, we should try to match an int first. *)
let float_pattern = @"[0-9]+(\.[0-9]+)?"

// TODO1 #parsing Keep a list of reserved words so the author cannot name characters after them. Extract from command_patterns_1. Add all commands not covered there.

let multi_line_comment_start_regex = @"^/\*.*$" |> get_compiled_regex
let multi_line_comment_end_regex = @".*?\*/$" |> get_compiled_regex

// TODO2 #parsing Consider reusing word/int/float patterns in these where applicable.

let dialogue_regex = @"^(?<character>\w+)\s+(?<dialogue>.+)$" |> get_compiled_regex

(* We use curly braces to delimit interpolations, so the author cannot, for instance, write an interpolation that contains the following.
function (x) { return x; }
(end)
They will need to define functions elsewhere, but they can call them here.
*)
let javascript_interpolation_regex = @"\{(?<interpolation>[^}]+)\}" |> get_compiled_regex

let single_line_comment_regex = @"^//.*$" |> get_compiled_regex

let menu_start_regex = @"^menu$" |> get_compiled_regex
let menu_start_with_parameters_regex = @"^menu\s+(?<name>\w+)\s+(?<description>.+)$" |> get_compiled_regex
let menu_item_regex = @"^(?<value>\d+)\s+(?<description>[^\$]+?)(?:\s+(\$if\s+(?<conditional>.+)))?$" |> get_compiled_regex
let end_menu_regex = @"^endmenu$" |> get_compiled_regex

let image_map_start_regex = @"^imagemap$" |> get_compiled_regex
let image_map_start_with_parameters_regex = @$"^imagemap\s+(?<name>\w+)\s+(?<background>\w+)\s+(?<transition_time>{float_pattern})$" |> get_compiled_regex
let image_map_item_regex = @"^(?<value>\d+)\s+(?<x1>\d+)\s+(?<y1>\d+)\s+(?<x2>\d+)\s+(?<y2>\d+)(?:\s+(\$if\s+(?<conditional>.+)))?$" |> get_compiled_regex
let end_image_map_regex = "^endimagemap$" |> get_compiled_regex
let end_image_map_with_parameters_regex = @$"^endimagemap\s+(?<transition_time>{float_pattern})$" |> get_compiled_regex

let javascript_start_regex = @"^js$" |> get_compiled_regex
let javascript_end_regex = @"^endjs$" |> get_compiled_regex
