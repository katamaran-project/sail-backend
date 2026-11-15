module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module TC = TranslationContext
open Monads.Notations.Star(TC)


let translate_kind (kind : S.kind) : Ast.Kind.t TC.t =
  let S.K_aux (kind, _location) = kind
  in
  match kind with
  | K_type -> TC.return @@ Ast.Kind.Type
  | K_int  -> TC.return @@ Ast.Kind.Int
  | K_bool -> TC.return @@ Ast.Kind.Bool


let translate_kind_id (kid : S.kid) : Ast.Identifier.t TC.t =
  TC.return @@ Identifier.translate_kind_identifier kid


let translate_type_quantifier_item (quantifier_item : Libsail.Ast.quant_item) :(Ast.Identifier.t * Ast.Kind.t) option TC.t =
  let S.QI_aux (unwrapped_quantifier_item, _location) = quantifier_item
  in
  match unwrapped_quantifier_item with
  | QI_constraint _numeric_constraint -> begin
      let* () =
        let message = lazy begin
          PP.format "Ignoring constraint %s" (StringOf.Sail.quant_item quantifier_item)
        end
        in
        TC.log [%here] Logging.info message
      in
      TC.return None
    end
  | QI_id (KOpt_aux (KOpt_kind (kind, kind_id), _loc)) -> begin
      let* kind'    = translate_kind kind
      and* kind_id' = translate_kind_id kind_id
      in
      TC.return @@ Some (kind_id', kind')
    end


let translate_type_quantifier (type_quantifier : Libsail.Ast.typquant) : Ast.TypeQuantifier.t TC.t =
  let* items = TC.filter_map ~f:translate_type_quantifier_item type_quantifier
  in
  TC.return @@ Ast.TypeQuantifier.TypeQuantifier items


(*
   The constraints of a type quantifier, as a list of conjuncts (forall 'n, 0 < 'n <= 8 & 'p == 8 * 'n
   gives 0 < 'n, 'n <= 8 and 'p == 8 * 'n). A conjunct that does not translate is left out
   (with a message): fewer hypotheses are weaker, hence sound.
*)
let translate_type_quantifier_constraints (type_quantifier : Libsail.Ast.typquant) : Ast.Numeric.Constraint.t list TC.t =
  let rec conjuncts (numeric_constraint : S.n_constraint) : S.n_constraint list =
    let S.NC_aux (unwrapped_numeric_constraint, _location) = numeric_constraint
    in
    match unwrapped_numeric_constraint with
    | NC_and (left, right) -> ExtBase.List.append (conjuncts left) (conjuncts right)
    | NC_equal _ | NC_not_equal _ | NC_ge _ | NC_gt _ | NC_le _ | NC_lt _ | NC_set _ | NC_or _
    | NC_app _ | NC_var _ | NC_id _ | NC_true | NC_false -> [ numeric_constraint ]
  in
  let translate_conjunct (numeric_constraint : S.n_constraint) : Ast.Numeric.Constraint.t option TC.t =
    TC.recover
      (TC.lift ~f:ExtBase.Option.some @@ Numeric.translate_numeric_constraint numeric_constraint)
      (fun _ ->
         let* () =
           let message = lazy begin
             PP.format "Constraint %s left out of the hypotheses: it does not translate" (Libsail.Ast_util.string_of_n_constraint numeric_constraint)
           end
           in
           TC.log [%here] Logging.info message
         in
         TC.return None)
  in
  let numeric_constraints =
    ExtBase.List.filter_map type_quantifier ~f:(fun (S.QI_aux (item, _)) ->
        match item with
        | QI_constraint numeric_constraint -> Some numeric_constraint
        | QI_id _                          -> None
      )
  in
  TC.filter_map ~f:translate_conjunct (ExtBase.List.concat_map ~f:conjuncts numeric_constraints)
