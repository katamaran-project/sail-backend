open Base

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast

module TC = TranslationContext
open Monads.Notations.Star(TC)
open Function


let translate_kind (kind : S.kind) : N.kind TC.t =
  let S.K_aux (kind, _location) = kind
  in
  match kind with
  | S.K_type -> TC.return @@ Ast.Kind_type
  | S.K_int  -> TC.return @@ Ast.Kind_int
  | S.K_bool -> TC.return @@ Ast.Kind_bool


let translate_kind_id (kid : S.kid) : N.identifier TC.t =
  let S.Kid_aux (Var kind_id, _id_loc) = kid
  in
  TC.return @@ Id.mk kind_id


let translate_type_quantifier_item (S.QI_aux (quantifier_item, location)) =
  match quantifier_item with
  | S.QI_id (KOpt_aux (KOpt_kind (kind, kind_id), _loc)) ->
    let* kind'    = translate_kind kind
    and* kind_id' = translate_kind_id kind_id
    in
    TC.return @@ (kind_id', kind')
  | S.QI_constraint _ -> TC.not_yet_implemented [%here] location


let translate_type_quantifier (S.TypQ_aux (quantifier, _location)) =
  match quantifier with
  | S.TypQ_tq items  -> TC.map ~f:translate_type_quantifier_item items
  | S.TypQ_no_forall -> TC.return @@ []
