open Base


module Subst = struct
  let rec numeric_expression (subst : Ast.Identifier.t -> Ast.Identifier.t) =
    let rec aux (nexp : Ast.NumericExpression.t) =
      match nexp with
      | Constant _           -> nexp
      | Add (left, right)    -> Add (aux left, aux right)
      | Minus (left, right)  -> Minus (aux left, aux right)
      | Times (left, right)  -> Times (aux left, aux right)
      | Neg operand          -> Neg (aux operand)
      | Id identifier        -> Id (subst identifier)
      | Var identifier       -> Var (subst identifier)
    in
    aux

  and nanotype (subst : Ast.Identifier.t -> Ast.Identifier.t) =
    let rec aux (t : Ast.Type.t) : Ast.Type.t =
      match t with
      | Unit                    -> Unit
      | Bool                    -> Bool
      | Nat                     -> Nat
      | Int                     -> Int
      | String                  -> String
      | Atom                    -> Atom
      | List x                  -> List (aux x)
      | Bitvector nexpr         -> Bitvector (numeric_expression subst nexpr)
      | Tuple ts                -> Tuple (List.map ~f:aux ts)
      | Application (id, targs) -> Application (id, List.map ~f:(type_argument subst) targs) (* id should probably not be substituted *)
      | Custom id               -> Custom id (* probably should not be substituted *)
      | Record                  -> Record (* todo *)
      | Enum id                 -> Enum id
      | Product (t1, t2)        -> Product (aux t1, aux t2)
      | Sum (t1, t2)            -> Sum (aux t1, aux t2)
    in
    aux

  and type_argument (subst : Ast.Identifier.t -> Ast.Identifier.t) =
    let aux (targ : Ast.TypeArgument.t) : Ast.TypeArgument.t =
      match targ with
      | Type t                 -> Type (nanotype subst t)
      | NumericExpression nexp -> NumericExpression (numeric_expression subst nexp)
      | Bool nconstr           -> Bool (numeric_constraint subst nconstr)
    in
    aux

  and numeric_constraint (subst : Ast.Identifier.t -> Ast.Identifier.t) =
    let rec aux (nconstr : Ast.NumericConstraint.t) : Ast.NumericConstraint.t =
      match nconstr with
      | Equal     (left, right)  -> Equal     (numeric_expression subst left, numeric_expression subst right)
      | BoundedGE (left, right)  -> BoundedGE (numeric_expression subst left, numeric_expression subst right)
      | BoundedGT (left, right)  -> BoundedGT (numeric_expression subst left, numeric_expression subst right)
      | BoundedLE (left, right)  -> BoundedLE (numeric_expression subst left, numeric_expression subst right)
      | BoundedLT (left, right)  -> BoundedLT (numeric_expression subst left, numeric_expression subst right)
      | NotEqual  (left, right)  -> NotEqual  (numeric_expression subst left, numeric_expression subst right)
      | Set (identifier, ns)     -> Set (subst identifier, ns)
      | Or (left, right)         -> Or (aux left, aux right)
      | And (left, right)        -> And (aux left, aux right)
      | App (identifier, targs)  -> App (identifier, List.map ~f:(type_argument subst) targs)
      | Var identifier           -> Var (subst identifier)
      | True                     -> True
      | False                    -> False
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
    type t = Ast.Identifier.t Ast.Identifier.Map.t

    let empty = Ast.Identifier.Map.empty

    let add = Ast.Identifier.Map.add_exn

    let find = Ast.Identifier.Map.find

    let contains_value map identifier =
      Ast.Identifier.Map.exists map ~f:(Ast.Identifier.equal identifier)
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
    (type_quantifier : Ast.Definition.type_quantifier             ) =
  let open SubstitutionMonad in
  let open Monads.Notations.Star(SubstitutionMonad)
  in
  let rec aux (items : Ast.Definition.type_quantifier_item list) =
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
    (type_quantifier : Ast.Definition.type_quantifier                    )
    (x               : 'a                                                ) =
  let type_quantifier', subst = process_type_quantifier sanitize type_quantifier
  in
  let x' = substituter subst x
  in
  (type_quantifier', x')


module Sanitize = struct
  let numeric_expression
      (type_quantifier    : Ast.Definition.type_quantifier)
      (numeric_expression : Ast.NumericExpression.t       ) =
    generic_sanitize
      sanitize_identifier
      Subst.numeric_expression
      type_quantifier
      numeric_expression

  let numeric_constraint
      (type_quantifier    : Ast.Definition.type_quantifier)
      (numeric_constraint : Ast.NumericConstraint.t       ) =
    generic_sanitize
      sanitize_identifier
      Subst.numeric_constraint
      type_quantifier
      numeric_constraint

  let nanotype
      (type_quantifier    : Ast.Definition.type_quantifier)
      (nanotype           : Ast.Type.t                    ) =
    generic_sanitize
      sanitize_identifier
      Subst.nanotype
      type_quantifier
      nanotype
end
