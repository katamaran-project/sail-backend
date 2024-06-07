open Base


type ocaml_source_location = Lexing.position


let string_of_position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : ocaml_source_location) =
  Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum


let rec string_of_location (location : Libsail.Parse_ast.l) =
  match location with
  | Unknown                 -> "UnknownLocation"
  | Unique (k, loc)         -> Printf.sprintf "UniqueLocation(%d, %s)" k (string_of_location loc)
  | Generated loc           -> Printf.sprintf "GeneratedLocation(%s)" (string_of_location loc)
  | Hint (hint, loc1, loc2) -> Printf.sprintf "HintLocation(%s, %s, %s)" hint (string_of_location loc1) (string_of_location loc2)
  | Range (pos1, pos2)      -> Printf.sprintf "Range(%s-%s)" (string_of_position pos1) (string_of_position pos2)
