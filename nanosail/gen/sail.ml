open PPrint
open Ast
open Util
open Monad

module PP = PPrint


let string_of_position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
  Printf.sprintf "Pos(%s:%d:%d:%d)" pos_fname pos_lnum pos_bol pos_cnum

let rec string_of_location (location : Libsail.Parse_ast.l) =
  match location with
  | Unknown                 -> "UnknownLocation"
  | Unique (k, loc)         -> Printf.sprintf "UniqueLocation(%d, %s)" k (string_of_location loc)
  | Generated loc           -> Printf.sprintf "GeneratedLocation(%s)" (string_of_location loc)
  | Hint (hint, loc1, loc2) -> Printf.sprintf "HintLocation(%s, %s, %s)" hint (string_of_location loc1) (string_of_location loc2)
  | Range (pos1, pos2)      -> Printf.sprintf "Range(%s-%s)" (string_of_position pos1) (string_of_position pos2)

let pp_numeric_expression (numeric_expression : numeric_expression) =
  let rec pp level numexp =
    let parens_if lvl doc =
      if level <= lvl
      then doc
      else parens doc
    in
    match numexp with
    | NE_constant z   -> string (Big_int.to_string z)
    | NE_add (x, y)   -> parens_if 0 @@ concat [ pp 0 x; space; plus; space; pp 0 y ]
    | NE_minus (x, y) -> parens_if 0 @@ concat [ pp 0 x; space; minus; space; pp 0 y ]
    | NE_times (x, y) -> parens_if 1 @@ concat [ pp 1 x; space; star; space; pp 1 y ]
    | NE_neg x        -> parens_if 2 @@ concat [ minus; pp 3 x ]
    | NE_id id        -> string id
    | NE_var id       -> string id
  in
  generate @@ pp 0 numeric_expression

and pp_numeric_constraint (numeric_constraint : numeric_constraint) =
  match numeric_constraint with
  | NC_equal (_x, _y)      -> not_yet_implemented __POS__
  | NC_bounded_ge (_x, _y) -> not_yet_implemented __POS__
  | NC_bounded_gt (_x, _y) -> not_yet_implemented __POS__
  | NC_bounded_le (_x, _y) -> not_yet_implemented __POS__
  | NC_bounded_lt (_x, _y) -> not_yet_implemented __POS__
  | NC_not_equal (_x, _y)  -> not_yet_implemented __POS__
  | NC_set (_x, _y)        -> not_yet_implemented __POS__
  | NC_or (_x, _y)         -> not_yet_implemented __POS__
  | NC_and (_x, _y)        -> not_yet_implemented __POS__
  | NC_app (_x, _y)        -> not_yet_implemented __POS__
  | NC_var _               -> not_yet_implemented __POS__
  | NC_true                -> generate @@ string "true"
  | NC_false               -> generate @@ string "false"

let rec pp_nanotype (typ : nanotype) =
  let pp_product x y =
    parens @@ simple_app [ string "ty.prod"; x; y ]
  in
  let pp_tuple elts =
    let* elts' = Monad.map pp_nanotype elts
    in
    match Auxlib.split_last elts' with
    | Some (xs, last) -> generate @@ List.fold_right pp_product xs last
    | None            -> not_yet_implemented __POS__
  in
  let pp_list element_type =
    let* element_type' = pp_nanotype element_type
    in
    generate @@ parens @@ simple_app [ string "ty.list"; element_type' ]
  in
  let pp_application id type_arguments =
    let id' = string id
    in
    let* type_arguments' =
      map pp_type_argument type_arguments
    in
    generate @@ parens @@ simple_app (id' :: type_arguments')
  in
  let pp_bitvector nexpr =
    let* nexpr' = pp_numeric_expression nexpr
    in
    generate @@ simple_app [ string "ty.bitvector"; nexpr' ]
  in
  match typ with
   | Ty_unit            -> generate @@ string "ty.unit"
   | Ty_bool            -> generate @@ string "ty.bool"
   | Ty_int             -> generate @@ string "ty.int"
   | Ty_string          -> generate @@ string "ty.string"
   | Ty_atom            -> generate @@ string "ty.atom"
   | Ty_custom id       -> generate @@ string id
   | Ty_list typ        -> pp_list typ
   | Ty_tuple ts        -> pp_tuple ts
   | Ty_app (id, targs) -> pp_application id targs
   | Ty_bitvector nexpr -> pp_bitvector nexpr


and pp_type_argument (type_argument : type_argument) =
  match type_argument with
  | TA_type t   -> pp_nanotype t
  | TA_numexp e -> pp_numeric_expression e
  | TA_bool nc  -> pp_numeric_constraint nc

let pp_bind (arg, t) =
  let* t' = pp_nanotype t in
  generate @@ utf8string ("\"" ^ arg ^ "\" ∷ " ) ^^ t'

let pp_sail_definition sail_definition =
  Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)

let pp_kind (kind : kind) =
  match kind with
  | Kind_type -> not_yet_implemented __POS__
  | Kind_int  -> generate @@ string @@ "nat"
  | Kind_bool -> not_yet_implemented __POS__

let pp_type_quantifier quantifier =
  let pp_type_quantifier_item (identifier, kind) =
    let identifier' = string identifier
    in
    let* kind' = pp_kind kind
    in
    generate @@ parens @@ separate space [
      identifier';
      colon;
      kind'
    ]
  in
  map pp_type_quantifier_item quantifier
