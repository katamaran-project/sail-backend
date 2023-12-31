open Base
open PPrint
open Ast
open Util
open Annotation_monad
open Monads.Notations.Star(Annotation_monad)

module PP = PPrint

let pp_identifier = string

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
    | NE_id id        -> pp_identifier id
    | NE_var id       -> pp_identifier id
  in
  return @@ pp 0 numeric_expression

and pp_numeric_constraint (numeric_constraint : numeric_constraint) =
  match numeric_constraint with
  | NC_equal (_x, _y)      -> not_yet_implemented [%here]
  | NC_bounded_ge (_x, _y) -> not_yet_implemented [%here]
  | NC_bounded_gt (_x, _y) -> not_yet_implemented [%here]
  | NC_bounded_le (_x, _y) -> not_yet_implemented [%here]
  | NC_bounded_lt (_x, _y) -> not_yet_implemented [%here]
  | NC_not_equal (_x, _y)  -> not_yet_implemented [%here]
  | NC_set (_x, _y)        -> not_yet_implemented [%here]
  | NC_or (_x, _y)         -> not_yet_implemented [%here]
  | NC_and (_x, _y)        -> not_yet_implemented [%here]
  | NC_app (_x, _y)        -> not_yet_implemented [%here]
  | NC_var _               -> not_yet_implemented [%here]
  | NC_true                -> return @@ string "true"
  | NC_false               -> return @@ string "false"

let rec pp_nanotype (typ : nanotype) =
  let pp_product x y =
    parens @@ simple_app [ pp_identifier "ty.prod"; x; y ]
  in
  let pp_tuple elts =
    let* elts' = Annotation_monad.map pp_nanotype elts
    in
    match Auxlib.split_last elts' with
    | Some (xs, last) -> return @@ List.fold_right ~f:pp_product ~init:last xs
    | None            -> not_yet_implemented [%here]
  in
  let pp_list element_type =
    let* element_type' = pp_nanotype element_type
    in
    return @@ parens @@ simple_app [ string "ty.list"; element_type' ]
  in
  let pp_application id type_arguments =
    let id' = pp_identifier id
    in
    let* type_arguments' =
      map pp_type_argument type_arguments
    in
    return @@ parens @@ simple_app (id' :: type_arguments')
  in
  let pp_bitvector nexpr =
    let* nexpr' = pp_numeric_expression nexpr
    in
    return @@ simple_app [ string "ty.bitvector"; nexpr' ]
  in
  match typ with
   | Ty_unit            -> return @@ string "ty.unit"
   | Ty_bool            -> return @@ string "ty.bool"
   | Ty_int             -> return @@ string "ty.int"
   | Ty_nat             -> return @@ string "ty.nat"
   | Ty_string          -> return @@ string "ty.string"
   | Ty_atom            -> return @@ string "ty.atom"
   | Ty_custom id       -> return @@ pp_identifier id
   | Ty_list typ        -> pp_list typ
   | Ty_tuple ts        -> pp_tuple ts
   | Ty_app (id, targs) -> pp_application id targs
   | Ty_bitvector nexpr -> pp_bitvector nexpr

and coq_type_of_nanotype (nanotype : nanotype) =
  match nanotype with
  | Ty_unit              -> return @@ string "Datatypes.unit"
  | Ty_bool              -> return @@ string "Datatypes.bool"
  | Ty_nat               -> return @@ string "nat"
  | Ty_int               -> return @@ string "Z"
  | Ty_string            -> return @@ string "String.string"
  | Ty_atom              -> not_yet_implemented [%here]
  | Ty_list _t           -> not_yet_implemented [%here]
  | Ty_bitvector n       -> let* n' = pp_numeric_expression n in return @@ string "bv" ^^ space ^^ n'
  | Ty_tuple _ts         -> not_yet_implemented [%here]
  | Ty_app (_t1, _t2)    -> not_yet_implemented [%here]
  | Ty_custom _id        -> not_yet_implemented [%here]

and pp_type_argument (type_argument : type_argument) =
  match type_argument with
  | TA_type t   -> pp_nanotype t
  | TA_numexp e -> pp_numeric_expression e
  | TA_bool nc  -> pp_numeric_constraint nc

let pp_bind (arg, t) =
  let* t' = pp_nanotype t in
  return @@ utf8string ("\"" ^ arg ^ "\" ∷ " ) ^^ t'

let pp_sail_definition sail_definition =
  Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)

let pp_kind (kind : kind) =
  match kind with
  | Kind_type -> not_yet_implemented [%here]
  | Kind_int  -> return @@ string @@ "nat"
  | Kind_bool -> not_yet_implemented [%here]

let pp_type_quantifier quantifier =
  let pp_type_quantifier_item (identifier, kind) =
    let identifier' = pp_identifier identifier
    in
    let* kind' = pp_kind kind
    in
    return @@ parens @@ separate space [
      identifier';
      colon;
      kind'
    ]
  in
  map pp_type_quantifier_item quantifier
