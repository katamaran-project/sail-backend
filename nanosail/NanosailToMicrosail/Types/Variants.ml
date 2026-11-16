open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(* Name for the inductive type listing all variant/union types *)
let variants_inductive_type_identifier = Ast.Identifier.mk "Unions"


(*
   The parameters of a variant: those of a width family (see SailToNanosail.UnionInstances),
   Inductive MemoryOpResult_bits (kid__n : nat), whose tag in Unions takes them too.
*)
let parameter_identifiers (variant_definition : Ast.Definition.Type.Variant.t) : Ast.Identifier.t list =
  let Ast.TypeQuantifier.TypeQuantifier items = variant_definition.type_quantifier
  in
  List.map ~f:fst items


let is_family (variant_definition : Ast.Definition.Type.Variant.t) : bool =
  let Ast.TypeQuantifier.TypeQuantifier items = variant_definition.type_quantifier
  in
  not (List.is_empty items) && List.for_all items ~f:(fun (_, kind) -> match kind with Int -> true | Type | Bool -> false)


(* name applied to the parameters of the variant: Ures_bits kid__n, MemoryOpResult_bits kid__n *)
let applied_to_parameters (variant_definition : Ast.Definition.Type.Variant.t) (name : PP.t) : PP.t =
  PP.separate_horizontally ~separator:PP.space (name :: List.map ~f:Identifier.pp (parameter_identifiers variant_definition))


(* The tag of the variant in Unions, as a pattern binding its parameters *)
let tag_pattern (variant_definition : Ast.Definition.Type.Variant.t) : PP.t =
  applied_to_parameters variant_definition (Identifier.pp @@ Identifier.reified_variant_name variant_definition.identifier)


(* The Rocq type of the variant at the parameters bound by tag_pattern *)
let denoted_type (variant_definition : Ast.Definition.Type.Variant.t) : PP.t =
  applied_to_parameters variant_definition (Identifier.pp variant_definition.identifier)


let generate_inductive_type (variant_definition : Ast.Definition.Type.Variant.t) : PP.t GC.t =
  let { identifier; type_quantifier; constructors } : Ast.Definition.Type.Variant.t = variant_definition
  in
  let inductive_type =
    let identifier' =
      Identifier.pp identifier
    in
    let pp_constructor_types (field_nanotypes : Ast.Type.t list) =
      let* ts = GC.map ~f:Type.coq_type_of_nanotype field_nanotypes
      in
      let ts = ts @ [ denoted_type variant_definition ]
      in
      GC.return @@ PP.separate_horizontally ~separator:(PP.string " -> ") ts
    in
    let* type_quantifier' =
      let Ast.TypeQuantifier.TypeQuantifier items = type_quantifier
      in
      GC.map items ~f:(fun (id, kind) ->
          let* kind' = Kind.pp_kind kind
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
    Printf.sprintf "Union Inductive Type for %s" @@ Ast.Identifier.to_string variant_definition.identifier
  in
  (* the constructors of a family take its parameters implicitly: Val_bits v, not Val_bits n v *)
  let arguments =
    match parameter_identifiers variant_definition with
    | []         -> []
    | parameters -> begin
        List.map constructors ~f:(fun (constructor, _) ->
            PP.string @@ Printf.sprintf "#[global] Arguments %s {%s}."
              (Ast.Identifier.to_string constructor)
              (String.concat ~sep:" " @@ List.map ~f:Ast.Identifier.to_string parameters)
          )
      end
  in
  GC.generation_block [%here] block_label @@ begin
    GC.block begin
      let* inductive_type = inductive_type
      in
      GC.return @@ PP.vertical (inductive_type :: arguments)
    end
  end


let derive_constructor_tags (variant_definition : Ast.Definition.Type.Variant.t) : Ast.Identifier.t list =
  let constructor_names = List.map ~f:fst variant_definition.constructors
  in
  List.map ~f:Identifier.reified_variant_constructor_name constructor_names


let generate_constructors_inductive_type (variant_definition : Ast.Definition.Type.Variant.t) : PP.t GC.t =
  let identifier = Identifier.pp @@ Identifier.reified_variant_constructors_collection_name variant_definition.identifier
  and typ        = Identifier.pp @@ Ast.Identifier.mk "Set"
  and tags       = derive_constructor_tags variant_definition
  in
  GC.generation_block [%here] (Printf.sprintf "Constructors Inductive Type for %s" @@ Ast.Identifier.to_string variant_definition.identifier) @@ begin
    GC.block begin
      GC.pp_inductive_type identifier typ @@ fun add_constructor -> begin
        GC.iter ~f:(fun tag -> add_constructor @@ Identifier.pp tag) tags
      end
    end
  end


let pp_variant_definition (variant_definition : Ast.Definition.Type.Variant.t) : PP.t GC.t =
  GC.generation_block [%here] "Variant Definition" begin
    let* inductive_type =
      generate_inductive_type variant_definition
    and* constructors_inductive_type =
      generate_constructors_inductive_type variant_definition
    in
    GC.return @@ PP.paragraphs [ inductive_type; constructors_inductive_type ]
  end


let generate_tags (variant_definitions : (Sail.sail_definition * Ast.Definition.Type.Variant.t) list) : PP.t GC.t =
  let variant_definitions =
    Ast.Definition.Select.drop_sail_definitions variant_definitions
  in
  let identifier = Identifier.pp variants_inductive_type_identifier
  and typ = PP.string "Set"
  and tag_of_variant (variant_definition : Ast.Definition.Type.Variant.t) =
    let id = Identifier.reified_variant_name variant_definition.identifier
    in
    Identifier.pp id
  and tag_parameters (variant_definition : Ast.Definition.Type.Variant.t) : PP.t =
    PP.separate_horizontally ~separator:PP.space @@ List.map (parameter_identifiers variant_definition) ~f:(fun identifier ->
        PP.(surround parens @@ separate_horizontally ~separator:space [ Identifier.pp identifier; colon; string "nat" ])
      )
  in
  GC.block begin
    GC.pp_inductive_type
      identifier
      typ
      (
        fun add_constructor -> begin
            GC.iter
              ~f:(fun variant_definition ->
                  add_constructor ~parameters:(tag_parameters variant_definition) @@ tag_of_variant variant_definition
                )
              variant_definitions
          end
      )
  end


let generate_tag_match
    ~(matched_identifier   : Ast.Identifier.t                                   )
    ~(variant_definitions  : Ast.Definition.Type.Variant.t list                 )
    ~(variant_case_handler : Ast.Definition.Type.Variant.t -> (PP.t * PP.t) GC.t) : PP.t GC.t
  =
  let* cases = GC.map ~f:variant_case_handler variant_definitions
  in
  GC.return @@ Coq.pp_match (Identifier.pp matched_identifier) cases


let generate_constructor_match
    ~(matched_identifier       : Ast.Identifier.t                                        )
    ~(variant_definition       : Ast.Definition.Type.Variant.t                           )
    ~(constructor_case_handler : Ast.Identifier.t * Ast.Type.t list -> (PP.t * PP.t) GC.t) : PP.t GC.t
  =
  let* cases = GC.map ~f:constructor_case_handler variant_definition.constructors
  in
  GC.return @@ Coq.pp_match (Identifier.pp matched_identifier) cases


let generate_constructor_finiteness (variant_definition : Ast.Definition.Type.Variant.t) : PP.t =
  let identifier = Identifier.pp @@ Identifier.reified_variant_constructors_collection_name variant_definition.identifier
  and type_name  = Identifier.pp @@ Identifier.reified_variant_constructors_collection_name variant_definition.identifier
  and values     = List.map ~f:Identifier.pp @@ derive_constructor_tags variant_definition
  in
  Coq.pp_finite_instance ~identifier ~type_name ~values


let generate_finiteness (variant_definitions : (Sail.sail_definition * Ast.Definition.Type.Variant.t) list) =
  let definitions = Ast.Definition.Select.drop_sail_definitions variant_definitions
  in
  List.map ~f:generate_constructor_finiteness definitions
