open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


(* Name for the inductive type listing all variant/union types *)
let variants_inductive_type_identifier = Ast.Identifier.mk "Unions"
  

let generate_inductive_type (variant_definition : Ast.Definition.Type.Variant.t) : PP.document AC.t =
  let { identifier; type_quantifier; constructors } : Ast.Definition.Type.Variant.t = variant_definition
  in
  let inductive_type =
    let identifier' =
      Identifier.pp identifier
    in
    let pp_constructor_types (field_nanotypes : Ast.Type.t list) =
      let* ts = AC.map ~f:Nanotype.coq_type_of_nanotype field_nanotypes
      in
      let ts = ts @ [ identifier' ]
      in
      AC.return @@ PP.separate (PP.string " -> ") ts
    in
    let* type_quantifier' =
      AC.map type_quantifier ~f:(fun (id, kind) ->
          let* kind' = PPSail.pp_kind kind
          in
          AC.return (Identifier.pp id, kind')
        )
    in
    Coq.build_inductive_type
      identifier'
      ~parameters: type_quantifier'
      (PP.string "Set")
      begin
        fun add_constructor ->
        AC.iter constructors ~f:(fun (constructor, typ) ->
            let* typ' = pp_constructor_types typ
            in
            add_constructor ~typ:typ' (Identifier.pp constructor))
      end
  in
  inductive_type


let generate_constructors_inductive_type (variant_definition  : Ast.Definition.Type.Variant.t) =
  let identifier = Identifier.pp @@ TranslationSettings.derive_variant_constructor_type variant_definition.identifier
  and typ = Identifier.pp @@ Ast.Identifier.mk "Set"
  and constructor_names = List.map ~f:fst variant_definition.constructors
  in
  Coq.build_inductive_type identifier typ (fun add_constructor ->
      AC.iter constructor_names
        ~f:(fun case -> add_constructor @@ Identifier.pp @@ TranslationSettings.convert_constructor_name_to_tag case)
    )


let generate (variant_definition : Ast.Definition.Type.Variant.t) =
  let* inductive_type = generate_inductive_type variant_definition
  and* constructors_inductive_type = generate_constructors_inductive_type variant_definition
  in
  AC.return @@ PP.separate (PP.twice PP.hardline) [
                   inductive_type;
                   constructors_inductive_type
                 ]


let generate_tags (variant_definitions : (Sail.sail_definition * Ast.Definition.Type.Variant.t) list) =
  let variant_definitions =
    List.map ~f:snd variant_definitions
  in
  let identifier = Identifier.pp variants_inductive_type_identifier
  and typ = PP.string "Set"
  and tag_of_variant (variant_definition : Ast.Definition.Type.Variant.t) =
    let id = TranslationSettings.convert_variant_name_to_tag variant_definition.identifier
    in
    Identifier.pp id
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


let generate_no_confusions (variant_definitions : (Sail.sail_definition * Ast.Definition.Type.Variant.t) list) =
  let generate_derivation (variant_definition : Ast.Definition.Type.Variant.t) =
    Coq.derive_no_confusion_for variant_definition.identifier
  in
  List.map ~f:(Fn.compose generate_derivation snd) variant_definitions


let generate_tag_match
    ~(matched_identifier   : Ast.Identifier.t                                          )
    ~(variant_definitions  : Ast.Definition.Type.Variant.t list                        )
    ~(variant_case_handler : Ast.Definition.Type.Variant.t -> PP.document * PP.document) : PP.document
  =
  Coq.match' (Identifier.pp matched_identifier) @@ List.map ~f:variant_case_handler variant_definitions


let generate_constructor_match
    ~(matched_identifier       : Ast.Identifier.t                                               )
    ~(variant_definition       : Ast.Definition.Type.Variant.t                                  )
    ~(constructor_case_handler : Ast.Identifier.t * Ast.Type.t list -> PP.document * PP.document) : PP.document
  =
  Coq.match' (Identifier.pp matched_identifier) @@ List.map ~f:constructor_case_handler variant_definition.constructors


let required_eqdecs (variant_definitions : (Sail.sail_definition * Ast.Definition.Type.Variant.t) list) : Ast.Identifier.t list =
  let variant_identifiers =
    List.map ~f:(fun (_, vd) -> vd.identifier) variant_definitions
  in
  variants_inductive_type_identifier :: variant_identifiers
