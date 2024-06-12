open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let pp_bind (arg, t) =
  AC.return @@ PP.(utf8string ("\"" ^ (Ast.Identifier.string_of arg) ^ "\" ∷ " ) ^^ t)


let pp_sail_definition sail_definition =
  Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)


let pp_kind (kind : Ast.Kind.t) =
  match kind with
  | Type -> AC.not_yet_implemented [%here]
  | Int  -> AC.return @@ PP.string @@ "nat"
  | Bool -> AC.not_yet_implemented [%here]


let pp_type_quantifier quantifier =
  let pp_type_quantifier_item (identifier, kind) =
    let identifier' = Identifier.pp_identifier identifier
    in
    let* kind' = pp_kind kind
    in
    AC.return (identifier', Some kind')
  in
  AC.map ~f:pp_type_quantifier_item quantifier
