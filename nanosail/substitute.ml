open Ast


module Subst = struct
  let rec numeric_expression (subst : identifier -> identifier) =
    let rec aux (nexp : numeric_expression) =
      match nexp with
      | NE_constant _           -> nexp
      | NE_add (left, right)    -> NE_add (aux left, aux right)
      | NE_minus (left, right)  -> NE_minus (aux left, aux right)
      | NE_times (left, right)  -> NE_times (aux left, aux right)
      | NE_neg operand          -> NE_neg (aux operand)
      | NE_id identifier        -> NE_id (subst identifier)
      | NE_var identifier       -> NE_var (subst identifier)
    in
    aux

  and nanotype (subst : identifier -> identifier) =
    let rec aux (t : nanotype) =
      match t with
      | Ty_unit            -> Ty_unit
      | Ty_bool            -> Ty_bool
      | Ty_int             -> Ty_int
      | Ty_string          -> Ty_string
      | Ty_atom            -> Ty_atom
      | Ty_list x          -> Ty_list (aux x)
      | Ty_bitvector nexpr -> Ty_bitvector (numeric_expression subst nexpr)
      | Ty_tuple ts        -> Ty_tuple (List.map aux ts)
      | Ty_app (id, targs) -> Ty_app (id, List.map (type_argument subst) targs) (* id should probably not be substituted *)
      | Ty_custom id       -> Ty_custom id (* probably should not be substituted *)
    in
    aux

  and type_argument (subst : identifier -> identifier) =
    let aux (targ : type_argument) =
      match targ with
      | TA_type t       -> TA_type (nanotype subst t)
      | TA_numexp nexp  -> TA_numexp (numeric_expression subst nexp)
      | TA_bool nconstr -> TA_bool (numeric_constraint subst nconstr)
    in
    aux

  and numeric_constraint (subst : identifier -> identifier) =
    let rec aux (nconstr : numeric_constraint) =
      match nconstr with
      | NC_equal (left, right)      -> NC_equal (numeric_expression subst left, numeric_expression subst right)
      | NC_bounded_ge (left, right) -> NC_bounded_ge (numeric_expression subst left, numeric_expression subst right)
      | NC_bounded_gt (left, right) -> NC_bounded_gt (numeric_expression subst left, numeric_expression subst right)
      | NC_bounded_le (left, right) -> NC_bounded_le (numeric_expression subst left, numeric_expression subst right)
      | NC_bounded_lt (left, right) -> NC_bounded_lt (numeric_expression subst left, numeric_expression subst right)
      | NC_not_equal (left, right)  -> NC_not_equal (numeric_expression subst left, numeric_expression subst right)
      | NC_set (identifier, ns)     -> NC_set (subst identifier, ns)
      | NC_or (left, right)         -> NC_or (aux left, aux right)
      | NC_and (left, right)        -> NC_and (aux left, aux right)
      | NC_app (identifier, targs)  -> NC_app (identifier, List.map (type_argument subst) targs)
      | NC_var identifier           -> NC_var (subst identifier)
      | NC_true                     -> NC_true
      | NC_false                    -> NC_false
    in
    aux
end


let sanitizing_substitution (identifier : identifier) : identifier =
  Auxlib.drop_chars_while identifier (fun c -> c = '\'')

  
let sanitize_identifier (identifier : identifier) : identifier option =
  if String.starts_with ~prefix:"'" identifier
  then Some (Auxlib.drop_chars_while identifier (fun c -> c = '\''))
  else None


(* let sanitize_numeric_expression = *)
(*   substitute_identifiers_in_numeric_expression sanitizing_substitution *)


(* let sanitize_type_quantifier (type_quantifier : type_quantifier) = *)
(*   let rec aux (items : type_quantifier_item list) = *)
(*     match items with *)
(*     | [] -> return [] *)
(*     | (id, kind) :: rest -> let* kind' = substitute Subst.kind kind *)
