open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


(* todo rename *)
let pp_bind' (arg, t) =
  GC.return @@ PP.(utf8string ("\"" ^ (Ast.Identifier.string_of arg) ^ "\" ∷ " ) ^^ t)


(* todo rename *)
let pp_kind' (kind : Ast.Kind.t) : PP.document GC.t =
  match kind with
  | Type -> GC.not_yet_implemented [%here]
  | Int  -> GC.return @@ PP.string @@ "nat"
  | Bool -> GC.not_yet_implemented [%here]


(* todo rename *)
let pp_type_quantifier' quantifier =
  let pp_type_quantifier_item (identifier, kind) =
    let identifier' = Identifier.pp identifier
    in
    let* kind' = pp_kind' kind
    in
    GC.return (identifier', Some kind')
  in
  GC.map ~f:pp_type_quantifier_item quantifier


let pp_sail_definition sail_definition =
  Libsail.Pretty_print_sail.doc_def (Libsail.Type_check.strip_def sail_definition)


(* todo use GC comments *)
let annotate_with_original_definition
    (original    : Libsail.Type_check.tannot Libsail.Ast.def)
    (translation : PP.document                              ) : PP.document
  =
  if
    Configuration.(get include_original_code)
  then
    PP.(
      concat [
        Coq.original_sail_code @@ pp_sail_definition original;
        hardline;
        translation
      ]
    )
  else
    translation


let annotate_with_original_definitions originals translation =
  if
    Configuration.(get include_original_code)
  then
    PP.(
      concat begin
        Auxlib.build_list begin fun { add; _ } ->
          add @@ Coq.original_sail_codes (List.map ~f:pp_sail_definition originals);
          add hardline;
          add translation
        end
      end
    )
  else
    translation

