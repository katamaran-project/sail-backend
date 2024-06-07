open Base
open Ast


module Subst = struct
  let rec numeric_expression (subst : Ast.Identifier.t -> Ast.Identifier.t) =
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

  and nanotype (subst : Ast.Identifier.t -> Ast.Identifier.t) =
    let rec aux (t : nanotype) =
      match t with
      | Ty_unit            -> Ty_unit
      | Ty_bool            -> Ty_bool
      | Ty_nat             -> Ty_nat
      | Ty_int             -> Ty_int
      | Ty_string          -> Ty_string
      | Ty_atom            -> Ty_atom
      | Ty_list x          -> Ty_list (aux x)
      | Ty_bitvector nexpr -> Ty_bitvector (numeric_expression subst nexpr)
      | Ty_tuple ts        -> Ty_tuple (List.map ~f:aux ts)
      | Ty_app (id, targs) -> Ty_app (id, List.map ~f:(type_argument subst) targs) (* id should probably not be substituted *)
      | Ty_custom id       -> Ty_custom id (* probably should not be substituted *)
      | Ty_record          -> Ty_record (* todo *)
      | Ty_prod (t1, t2)   -> Ty_prod (aux t1, aux t2)
      | Ty_sum (t1, t2)    -> Ty_sum (aux t1, aux t2)
    in
    aux

  and type_argument (subst : Ast.Identifier.t -> Ast.Identifier.t) =
    let aux (targ : type_argument) =
      match targ with
      | TA_type t       -> TA_type (nanotype subst t)
      | TA_numexp nexp  -> TA_numexp (numeric_expression subst nexp)
      | TA_bool nconstr -> TA_bool (numeric_constraint subst nconstr)
    in
    aux

  and numeric_constraint (subst : Ast.Identifier.t -> Ast.Identifier.t) =
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
      | NC_app (identifier, targs)  -> NC_app (identifier, List.map ~f:(type_argument subst) targs)
      | NC_var identifier           -> NC_var (subst identifier)
      | NC_true                     -> NC_true
      | NC_false                    -> NC_false
    in
    aux
end


let remove_apostrophes_at_start =
  String.lstrip ~drop:(Char.equal '\'')

let sanitizing_substitution =
  remove_apostrophes_at_start

let sanitize_identifier (identifier : Ast.Identifier.t) : Ast.Identifier.t option =
  let s = Ast.Identifier.string_of identifier
  in
  if String.is_prefix ~prefix:"'" s
  then Some (Id (remove_apostrophes_at_start s))
  else None


module SubstitutionMonad = struct
  module SubstitutionMap = struct
    type t = Identifier.t Identifier.Map.t

    let empty = Identifier.Map.empty

    let add = Identifier.Map.add_exn

    let find = Identifier.Map.find

    let contains_value map identifier =
      Identifier.Map.exists map ~f:(Identifier.equal identifier)
  end

  include Monads.State.Make(SubstitutionMap)


  let add_substitution identifier identifier' =
    bind get (fun map ->
        let map' = SubstitutionMap.add map ~key:identifier ~data:identifier'
        in
        bind (put map') (fun () -> return ())
      )
end

(*
   Creates a function of type identifier -> identifier.
   that "translates" identifiers based on associations in the given map.

   If a given identifier is in the map, the function will return its associated key.

   If a given identifier is not in the map, the identifier is preserved.
   However, if this identifier is used as a value in the map,
   a clash occurs. For example, say you have substitution { a -> b },
   and wish to apply it on the expression a + b.
   Without clash-checking, this would result in b + b, which we
   want to avoid: distinct variables needs to remain as such.
*)
let create_substitution_from_map map =
  let open SubstitutionMonad
  in
  let contains_value (identifier : Ast.Identifier.t) =
    SubstitutionMap.contains_value map identifier
  in
  fun id ->
  match SubstitutionMap.find map id with
  | Some id' -> id'
  | None     -> if contains_value id then failwith "Clash!" else id

let process_type_quantifier
    (sanitize        : Ast.Identifier.t -> Ast.Identifier.t option)
    (type_quantifier : type_quantifier                            ) =
  let open SubstitutionMonad in
  let open Monads.Notations.Star(SubstitutionMonad)
  in
  let rec aux (items : type_quantifier_item list) =
    match items with
    | []                 -> return []
    | (id, kind) :: rest ->
      begin
        let* rest' = aux rest
        in
        match sanitize id with
        | Some id' -> let* () = add_substitution id id' in return @@ (id', kind) :: rest'
        | None     -> return @@ (id, kind) :: rest'
      end
  in
  let (type_quantifier', map) = run (aux type_quantifier) SubstitutionMap.empty
  in
  (type_quantifier', create_substitution_from_map map)

let generic_sanitize
    (sanitize        : Ast.Identifier.t -> Ast.Identifier.t option       )
    (substituter     : (Ast.Identifier.t -> Ast.Identifier.t) -> 'a -> 'a)
    (type_quantifier : type_quantifier                                   )
    (x               : 'a                                                ) =
  let type_quantifier', subst = process_type_quantifier sanitize type_quantifier
  in
  let x' = substituter subst x
  in
  (type_quantifier', x')


module Sanitize = struct
  let numeric_expression
      (type_quantifier    : type_quantifier   )
      (numeric_expression : numeric_expression) =
    generic_sanitize
      sanitize_identifier
      Subst.numeric_expression
      type_quantifier
      numeric_expression

  let numeric_constraint
      (type_quantifier    : type_quantifier   )
      (numeric_constraint : numeric_constraint) =
    generic_sanitize
      sanitize_identifier
      Subst.numeric_constraint
      type_quantifier
      numeric_constraint

  let nanotype
      (type_quantifier    : type_quantifier)
      (nanotype           : nanotype       ) =
    generic_sanitize
      sanitize_identifier
      Subst.nanotype
      type_quantifier
      nanotype
end
