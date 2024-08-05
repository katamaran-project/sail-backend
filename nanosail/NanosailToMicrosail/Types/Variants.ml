open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


(* Name for the inductive type listing all variant/union types *)
let variants_inductive_type_identifier = Ast.Identifier.mk "Unions"


let generate_inductive_type (variant_definition : Ast.Definition.Type.Variant.t) : PP.document GC.t =
  let { identifier; type_quantifier; constructors } : Ast.Definition.Type.Variant.t = variant_definition
  in
  let inductive_type =
    let identifier' =
      Identifier.pp identifier
    in
    let pp_constructor_types (field_nanotypes : Ast.Type.t list) =
      let* ts = GC.map ~f:Nanotype.coq_type_of_nanotype' field_nanotypes
      in
      let ts = ts @ [ identifier' ]
      in
      GC.return @@ PP.separate (PP.string " -> ") ts
    in
    let* type_quantifier' =
      GC.map type_quantifier ~f:(fun (id, kind) ->
          let* kind' = PPSail.pp_kind' kind
          in
          GC.return (Identifier.pp id, kind')
        )
    in
    GC.pp_inductive_type
      identifier'
      ~parameters: type_quantifier'
      (PP.string "Set")
      begin
        fun add_constructor ->
        GC.iter constructors ~f:(fun (constructor, typ) ->
            let* typ' = pp_constructor_types typ
            in
            add_constructor ~typ:typ' (Identifier.pp constructor))
      end
  in
  let block_label =
    PP.(string "Union Inductive Type for " ^^ Identifier.pp variant_definition.identifier)
  in
  GC.generation_block [%here] block_label @@* begin
    GC.block inductive_type
  end


let derive_constructor_tags (variant_definition : Ast.Definition.Type.Variant.t) : Ast.Identifier.t list =
  let constructor_names = List.map ~f:fst variant_definition.constructors
  in
  List.map ~f:Identifier.reified_variant_constructor_name constructor_names


let generate_constructors_inductive_type (variant_definition : Ast.Definition.Type.Variant.t) : PP.document GC.t =
  let identifier = Identifier.pp @@ Identifier.reified_variant_constructors_collection_name variant_definition.identifier
  and typ        = Identifier.pp @@ Ast.Identifier.mk "Set"
  and tags       = derive_constructor_tags variant_definition
  in
  GC.generation_block [%here] PP.(string "Constructors Inductive Type for" ^^ Identifier.pp variant_definition.identifier) @@* begin
    GC.block begin
      GC.pp_inductive_type identifier typ @@ fun add_constructor -> begin
        GC.iter ~f:(fun tag -> add_constructor @@ Identifier.pp tag) tags
      end
    end
  end


let generate (variant_definition : Ast.Definition.Type.Variant.t) : PP.document GC.t =
  let* inductive_type =
    generate_inductive_type variant_definition
  and* constructors_inductive_type =
    generate_constructors_inductive_type variant_definition
  in
  GC.return begin
    PP.vertical ~spacing:2 [
      inductive_type;
      constructors_inductive_type
    ]
  end


let generate_tags (variant_definitions : (Sail.sail_definition * Ast.Definition.Type.Variant.t) list) : PP.document GC.t =
  let variant_definitions =
    List.map ~f:snd variant_definitions
  in
  let identifier = Identifier.pp variants_inductive_type_identifier
  and typ = PP.string "Set"
  and tag_of_variant (variant_definition : Ast.Definition.Type.Variant.t) =
    let id = Identifier.reified_variant_name variant_definition.identifier
    in
    Identifier.pp id
  in
  GC.block begin
    GC.pp_inductive_type
      identifier
      typ
      (
        fun add_constructor -> begin
            GC.iter
              ~f:(fun variant_identifier ->
                  add_constructor @@ tag_of_variant variant_identifier
                )
              variant_definitions
          end
      )
  end


let generate_tag_match
    ~(matched_identifier   : Ast.Identifier.t                                                 )
    ~(variant_definitions  : Ast.Definition.Type.Variant.t list                               )
    ~(variant_case_handler : Ast.Definition.Type.Variant.t -> (PP.document * PP.document) GC.t) : PP.document GC.t
  =
  let* cases = GC.map ~f:variant_case_handler variant_definitions
  in
  GC.return @@ Coq.match' (Identifier.pp matched_identifier) cases


let generate_constructor_match
    ~(matched_identifier       : Ast.Identifier.t                                                      )
    ~(variant_definition       : Ast.Definition.Type.Variant.t                                         )
    ~(constructor_case_handler : Ast.Identifier.t * Ast.Type.t list -> (PP.document * PP.document) GC.t) : PP.document GC.t
  =
  let* cases = GC.map ~f:constructor_case_handler variant_definition.constructors
  in
  GC.return @@ Coq.match' (Identifier.pp matched_identifier) cases


(*

   Given a variant definition, returns which EqDecs are required.
   
 *)
let eqdec_identifiers_for (variant_definition : Ast.Definition.Type.Variant.t) : Ast.Identifier.t list =
  let inductive_type_identifier =
    variant_definition.identifier
  and constructors_inductive_type_identifier =
    Identifier.reified_variant_constructors_collection_name variant_definition.identifier
  in
  [ inductive_type_identifier; constructors_inductive_type_identifier ]


let extra_eqdec_identifiers () : Ast.Identifier.t list =
  [ variants_inductive_type_identifier ]


let no_confusion_identifiers_for (variant_definition : Ast.Definition.Type.Variant.t) : Ast.Identifier.t list =
  let inductive_type_identifier =
    variant_definition.identifier
  and constructors_inductive_type_identifier =
    Identifier.reified_variant_constructors_collection_name variant_definition.identifier
  in
  [ inductive_type_identifier; constructors_inductive_type_identifier ]


let extra_no_confusion_identifiers () =
  [ variants_inductive_type_identifier ]


let generate_constructor_finiteness (variant_definition : Ast.Definition.Type.Variant.t) : PP.document =
  let identifier = Identifier.pp @@ Identifier.reified_variant_constructors_collection_name variant_definition.identifier
  and type_name  = Identifier.pp @@ Identifier.reified_variant_constructors_collection_name variant_definition.identifier
  and values     = List.map ~f:Identifier.pp @@ derive_constructor_tags variant_definition
  in
  Coq.finite_instance ~identifier ~type_name ~values


let generate_finiteness (variant_definitions : (Sail.sail_definition * Ast.Definition.Type.Variant.t) list) =
  let definitions = List.map ~f:snd variant_definitions
  in
  List.map ~f:generate_constructor_finiteness definitions
