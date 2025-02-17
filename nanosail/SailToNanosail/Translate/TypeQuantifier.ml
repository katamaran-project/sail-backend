open! ExtBase

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
  let S.Kid_aux (Var unwappred_kid, _id_loc) = kid
  in
  TC.return @@ Ast.Identifier.mk unwappred_kid


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
        TC.log [%here] Logging.warning message
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
  let S.TypQ_aux (unwrapped_type_quantifier, _location) = type_quantifier
  in
  match unwrapped_type_quantifier with
  | TypQ_tq items  -> begin
      let* items = TC.filter_map ~f:translate_type_quantifier_item items
      in
      TC.return @@ Ast.TypeQuantifier.TypeQuantifier items
    end
  | TypQ_no_forall -> TC.return @@ Ast.TypeQuantifier.TypeQuantifier []
