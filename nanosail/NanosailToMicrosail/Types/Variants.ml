open Base
open PPrint
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module PP = PPrint


let generate_inductive_type (variant_definition : Ast.variant_definition) : PP.document AC.t =
  let { Ast.identifier; type_quantifier; constructors } = variant_definition
  in
  let inductive_type =
    let identifier' =
      pp_identifier identifier
    in
    let pp_constructor_types (field_nanotypes : Ast.Type.t list) =
      let* ts = AC.map ~f:Nanotype.coq_type_of_nanotype field_nanotypes
      in
      let ts = ts @ [ identifier' ]
      in
      AC.return @@ separate (string " -> ") ts
    in
    let* type_quantifier' =
      AC.map type_quantifier ~f:(fun (id, kind) ->
          let* kind' = PPSail.pp_kind kind
          in
          AC.return (pp_identifier id, kind')
        )
    in
    Coq.build_inductive_type
      identifier'
      ~parameters: type_quantifier'
      (string "Set")
      begin
        fun add_constructor ->
        AC.iter constructors ~f:(fun (constructor, typ) ->
            let* typ' = pp_constructor_types typ
            in
            add_constructor ~typ:typ' (pp_identifier constructor))
      end
  in
  inductive_type


let generate_constructors_inductive_type (variant_definition  : Ast.variant_definition) =
  let identifier = pp_identifier @@ TranslationSettings.derive_variant_constructor_type variant_definition.identifier
  and typ = pp_identifier @@ Ast.Identifier.mk "Set"
  and constructor_names = List.map ~f:fst variant_definition.constructors
  in
  Coq.build_inductive_type identifier typ (fun add_constructor ->
      AC.iter constructor_names
        ~f:(fun case -> add_constructor @@ pp_identifier @@ Ast.Identifier.add_prefix "K" case)
    )


let generate (variant_definition : Ast.variant_definition) =
  let* inductive_type = generate_inductive_type variant_definition
  and* constructors_inductive_type = generate_constructors_inductive_type variant_definition
  in
  AC.return @@ PP.separate (twice hardline) [
                   inductive_type;
                   constructors_inductive_type
                 ]


let generate_tags (variant_definitions : (Sail.sail_definition * Ast.variant_definition) list) =
  let variant_definitions =
    List.map ~f:snd variant_definitions
  in
  let identifier = PP.string "Unions"
  and typ = PP.string "Set"
  and tag_of_variant (variant_definition : Ast.variant_definition) =
    let id = TranslationSettings.convert_variant_name_to_tag variant_definition.identifier
    in
    pp_identifier id
  in
  let inductive_type =
    Coq.build_inductive_type
      identifier
      typ
      (fun add_constructor ->
        AC.iter
          ~f:(fun variant_identifier ->
            add_constructor @@ tag_of_variant variant_identifier
          )
          variant_definitions
      )
  in
  Coq.annotate inductive_type
