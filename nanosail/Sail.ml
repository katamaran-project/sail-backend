type type_annotation = Libsail.Type_check.tannot

type definition_annotation = Libsail.Type_check.env Libsail.Ast.def_annot

type sail_definition = (type_annotation, Libsail.Type_check.env) Libsail.Ast.def

type ast = (Libsail.Type_check.tannot, Libsail.Type_check.env) Libsail.Ast_defs.ast


let rec string_of_location (location : Libsail.Parse_ast.l) =
  let string_of_ocaml_position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
    Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum
  in
  match location with
  | Unknown                 -> "UnknownLocation"
  | Unique (k, loc)         -> Printf.sprintf "UniqueLocation(%d, %s)" k (string_of_location loc)
  | Generated loc           -> Printf.sprintf "GeneratedLocation(%s)" (string_of_location loc)
  | Hint (hint, loc1, loc2) -> Printf.sprintf "HintLocation(%s, %s, %s)" hint (string_of_location loc1) (string_of_location loc2)
  | Range (pos1, pos2)      -> Printf.sprintf "Range(%s-%s)" (string_of_ocaml_position pos1) (string_of_ocaml_position pos2)
