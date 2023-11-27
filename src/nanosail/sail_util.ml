open PPrint
open Ast
open Pputil
       
module PP = PPrint
module Coq = Coq_generation


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
  in
  pp 0 numeric_expression

let pp_ty_id = function
  | Unit      -> string "ty.unit"
  | Bool      -> string "ty.bool"
  | Int       -> string "ty.int"
  | String    -> string "ty.string"
  | List      -> string "ty.list"
  | Prod      -> string "ty.prod"
  | Bitvector -> string "ty.bvec"
  | Id_nys    -> string "TY_ID_" ^^ nys

let rec pp_ty = function
  | Ty_id (ty_id)         -> pp_ty_id ty_id
  | Ty_app (ty_id, targs) -> parens_app ((pp_ty_id ty_id) :: (List.map pp_type_argument targs))
  | Ty_nys                -> !^"TY_" ^^ nys
and pp_type_argument (type_argument : type_argument) =
  match type_argument with
  | TA_type t   -> pp_ty t
  | TA_numexp e -> pp_numeric_expression e

let pp_bind (arg, t) =
  utf8string ("\"" ^ arg ^ "\" ∷ " ) ^^ pp_ty t
