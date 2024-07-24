open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


(* Name for the inductive type listing all variant/union types *)
let variants_inductive_type_identifier = Ast.Identifier.mk "Unions"


let derive_variant_constructor_type_identifier =
  TranslationSettings.derive_variant_constructor_type_identifier


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


let derive_constructor_tag (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  TranslationSettings.convert_constructor_name_to_tag identifier


let derive_constructor_tags (variant_definition : Ast.Definition.Type.Variant.t) : Ast.Identifier.t list =
  let constructor_names = List.map ~f:fst variant_definition.constructors
  in
  List.map ~f:derive_constructor_tag constructor_names


let generate_constructors_inductive_type (variant_definition  : Ast.Definition.Type.Variant.t) =
  let identifier = Identifier.pp @@ derive_variant_constructor_type_identifier variant_definition.identifier
  and typ        = Identifier.pp @@ Ast.Identifier.mk "Set"
  and tags       = derive_constructor_tags variant_definition
  in
  Coq.build_inductive_type identifier typ @@ fun add_constructor -> begin
    AC.iter ~f:(fun tag -> add_constructor @@ Identifier.pp tag) tags
  end


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
      (
        fun add_constructor -> begin
            AC.iter
              ~f:(fun variant_identifier ->
                  add_constructor @@ tag_of_variant variant_identifier
                )
              variant_definitions
          end
      )
  in
  Coq.annotate inductive_type


let generate_tag_match
    ~(matched_identifier   : Ast.Identifier.t                                                 )
    ~(variant_definitions  : Ast.Definition.Type.Variant.t list                               )
    ~(variant_case_handler : Ast.Definition.Type.Variant.t -> (PP.document * PP.document) AC.t) : PP.document AC.t
  =
  let* cases = AC.map ~f:variant_case_handler variant_definitions
  in
  AC.return @@ Coq.match' (Identifier.pp matched_identifier) cases


let generate_constructor_match
    ~(matched_identifier       : Ast.Identifier.t                                                      )
    ~(variant_definition       : Ast.Definition.Type.Variant.t                                         )
    ~(constructor_case_handler : Ast.Identifier.t * Ast.Type.t list -> (PP.document * PP.document) AC.t) : PP.document AC.t
  =
  let* cases = AC.map ~f:constructor_case_handler variant_definition.constructors
  in
  AC.return @@ Coq.match' (Identifier.pp matched_identifier) cases


let collect_identifiers (variant_definitions : (Sail.sail_definition * Ast.Definition.Type.Variant.t) list) : Ast.Identifier.t list =
  let variant_identifiers =
    List.map ~f:(fun (_, vd) -> vd.identifier) variant_definitions
  in
  let variant_constructor_identifiers =
    List.map ~f:derive_variant_constructor_type_identifier variant_identifiers
  in
  Auxlib.build_list @@ fun { add; addall; _ } -> begin
    add    variants_inductive_type_identifier;
    addall variant_identifiers               ;
    addall variant_constructor_identifiers   ;
  end


let required_no_confusions = collect_identifiers
let required_eqdecs        = collect_identifiers


let generate_constructor_finiteness (variant_definition : Ast.Definition.Type.Variant.t) =
  let identifier = Identifier.pp @@ derive_variant_constructor_type_identifier variant_definition.identifier
  and type_name  = Identifier.pp @@ derive_variant_constructor_type_identifier variant_definition.identifier
  and values     = List.map ~f:Identifier.pp @@ derive_constructor_tags variant_definition
  in
  Coq.finite_instance ~identifier ~type_name ~values


let generate_finiteness (variant_definitions : (Sail.sail_definition * Ast.Definition.Type.Variant.t) list) =
  let definitions = List.map ~f:snd variant_definitions
  in
  List.map ~f:generate_constructor_finiteness definitions
