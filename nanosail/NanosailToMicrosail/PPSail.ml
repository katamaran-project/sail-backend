open Base
open Nanotype
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let pp_bind (arg, t) =
  let* t' = pp_nanotype t in
  AC.return @@ PP.(utf8string ("\"" ^ (Ast.Identifier.string_of arg) ^ "\" ∷ " ) ^^ t')


let pp_sail_definition sail_definition =
  Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)


let pp_kind (kind : Ast.Kind.t) =
  match kind with
  | Kind_type -> AC.not_yet_implemented [%here]
  | Kind_int  -> AC.return @@ PP.string @@ "nat"
  | Kind_bool -> AC.not_yet_implemented [%here]


let pp_type_quantifier quantifier =
  let pp_type_quantifier_item (identifier, kind) =
    let identifier' = Identifier.pp_identifier identifier
    in
    let* kind' = pp_kind kind
    in
    AC.return @@ PP.(parens @@ separate space [
      identifier';
      colon;
      kind'
    ])
  in
  AC.map ~f:pp_type_quantifier_item quantifier
