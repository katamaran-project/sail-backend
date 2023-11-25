open PPrint

let indent = nest 2
let indent' ?(level = 2) doc = blank level ^^ align doc

let small_step = twice hardline
let big_step   = twice small_step

let nys    = string "NOT_YET_SUPPORTED "
let ic     = string " IMPOSSIBLE_CASE "

let pp_delimited_sequence left_delimiter right_delimiter separator items =
  concat [
    left_delimiter;
    align (
      group (separate (separator ^^ break 1) items)
    );
    right_delimiter
  ]

(*
   Formats elements as

    x xs[0] xs[1] xs[2]

    or

    x xs[0]
      xs[1]
      xs[2]
*)
let pp_hanging_list ?(adaptive = true) x xs =
  if
    adaptive
  then
    concat [
      x;
      space;
      align (group (separate (break 1) xs))
    ]
  else
    concat [
      x;
      space;
      align (separate hardline xs)
    ]

let simple_app argv = indent (flow (break 1) argv)
let parens_app argv = parens (simple_app argv)

let pp_indented_enclosed_lines starting_line indented ending_line =
  separate hardline [
    starting_line;
    indent' indented;
    ending_line
  ]

let string_of_position (position : Lexing.position) =
  match position with
  | { pos_fname; pos_lnum; pos_bol; pos_cnum } ->
     Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum

let rec string_of_location (location : Libsail.Parse_ast.l) =
  match location with
  | Unknown -> "UnknownLocation"
  | Unique (k, loc) ->
     Printf.sprintf "UniqueLocation(%d, %s)" k (string_of_location loc)
  | Generated loc ->
     Printf.sprintf "GeneratedLocation(%s)" (string_of_location loc)
  | Hint (hint, loc1, loc2) ->
     Printf.sprintf "HintLocation(%s, %s, %s)" hint (string_of_location loc1) (string_of_location loc2)
  | Range (pos1, pos2) ->
     Printf.sprintf "Range(%s-%s)" (string_of_position pos1) (string_of_position pos2)

