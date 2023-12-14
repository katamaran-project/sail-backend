open PPrint
open Ast
open Util
open Monad

module PP = PPrint


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

let pp_numeric_expression (numeric_expression : numeric_expression) =
  let rec pp level numexp =
    let parens_if lvl doc =
      if level <= lvl
      then doc
      else parens doc
    in
    match numexp with
    | NE_constant z   -> string (Big_int.to_string z)
    | NE_add (x, y)   -> parens_if 0 (concat [ pp 0 x; space; plus; space; pp 0 y ])
    | NE_minus (x, y) -> parens_if 0 (concat [ pp 0 x; space; minus; space; pp 0 y ])
    | NE_times (x, y) -> parens_if 1 (concat [ pp 1 x; space; star; space; pp 1 y ])
    | NE_neg x        -> parens_if 2 (concat [ minus; pp 3 x ])
    | NE_id id        -> string id
    | NE_var id       -> string id
  in
  generate (pp 0 numeric_expression)

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
  | NC_true                -> generate (string "true")
  | NC_false               -> generate (string "false")

let pp_ty_id type_id =
  match type_id with
  | Unit                -> generate (string "ty.unit")
  | Bool                -> generate (string "ty.bool")
  | Int                 -> generate (string "ty.int")
  | String              -> generate (string "ty.string")
  | List                -> generate (string "ty.list")
  | Bitvector           -> generate (string "ty.bvec")
  | Atom                -> generate (string "ty.atom")
  | UserType identifier -> generate (string identifier)
  | Id_nys              -> not_yet_implemented __POS__


let rec pp_ty typ =
  let pp_product x y =
    parens (
      simple_app [
        string "ty.prod";
        x;
        y
      ]
    )
  in
  match typ with
  | Ty_id (ty_id)         -> pp_ty_id ty_id
  | Ty_nys                -> not_yet_implemented __POS__
  | Ty_tuple elts         ->
    begin
      let* elts' = Monad.map pp_ty elts
      in
      match Auxlib.split_last elts' with
        | Some (xs, last) -> generate (List.fold_right pp_product xs last)
        | None            -> not_yet_implemented __POS__
    end
  | Ty_app (ty_id, targs) ->
    begin
      let* first = pp_ty_id ty_id in
      let* rest  = seqmap (List.map pp_type_argument targs)
      in
      generate (parens_app (first :: rest))
    end

and pp_type_argument (type_argument : type_argument) =
  match type_argument with
  | TA_type t   -> pp_ty t
  | TA_numexp e -> pp_numeric_expression e
  | TA_bool nc  -> pp_numeric_constraint nc

let pp_bind (arg, t) =
  let* t' = pp_ty t in
  generate (utf8string ("\"" ^ arg ^ "\" ∷ " ) ^^ t')

let pp_sail_definition sail_definition =
  Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)
