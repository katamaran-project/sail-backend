open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


class katamaran (intermediate_representation : Ast.Program.t) = object(self : 'self)
  val all_definitions                          = intermediate_representation.definitions
  val type_definitions                         = Ast.Definition.Select.(select (with_sail_definition @@ type_definition of_anything         ) intermediate_representation.definitions)
  val enum_definitions                         = Ast.Definition.Select.(select (with_sail_definition @@ type_definition of_enum             ) intermediate_representation.definitions)
  val record_definitions                       = Ast.Definition.Select.(select (with_sail_definition @@ type_definition of_record           ) intermediate_representation.definitions)
  val variant_definitions                      = Ast.Definition.Select.(select (with_sail_definition @@ type_definition of_variant          ) intermediate_representation.definitions)
  val register_definitions                     = Ast.Definition.Select.(select (with_sail_definition @@ register_definition                 ) intermediate_representation.definitions)
  val ignored_definitions                      = Ast.Definition.Select.(select (with_sail_definition @@ ignored_definition                  ) intermediate_representation.definitions)
  val untranslated_definitions                 = Ast.Definition.Select.(select (with_sail_definition @@ untranslated_definition             ) intermediate_representation.definitions)
  val function_definitions                     = Ast.Definition.Select.(select (with_sail_definition @@ function_definition                 ) intermediate_representation.definitions)
  val top_level_type_constraint_definitions    = Ast.Definition.Select.(select (with_sail_definition @@ top_level_type_constraint_definition) intermediate_representation.definitions)

  method program                               = intermediate_representation
  method all_definitions                       = all_definitions
  method type_definitions                      = type_definitions
  method enum_definitions                      = enum_definitions
  method record_definitions                    = record_definitions
  method variant_definitions                   = variant_definitions
  method register_definitions                  = register_definitions
  method ignored_definitions                   = ignored_definitions
  method untranslated_definitions              = untranslated_definitions
  method function_definitions                  = function_definitions
  method top_level_type_constraint_definitions = top_level_type_constraint_definitions

  method pp_base_prelude : PP.t GC.t =
    GC.block begin
        GC.generation_block [%here] "Prelude" @@ begin
          BaseModule.generate_base_prelude ()
        end
      end

  method pp_program_prelude : PP.t GC.t =
    GC.block begin
        GC.generation_block [%here] "Prelude" @@ begin
          ProgramModule.generate_program_prelude ()
        end
      end

  method pp_register_definitions : PP.t GC.t =
    GC.block begin
        GC.generation_block [%here] "Register Definitions" @@ begin
          Registers.pp_regname_inductive_type register_definitions
        end
      end

  method pp_translated_type_definitions : PP.t GC.t =
    GC.block begin
        GC.generation_block [%here] "Translated Type Definitions" @@ begin
          let* type_definitions' =
            GC.map ~f:(Fn.uncurry Types.pp_type_definition) type_definitions
          in
          GC.return @@ PP.paragraphs type_definitions'
        end
      end

  method pp_enum_tags : PP.t GC.t =
    GC.block begin
        GC.generation_block [%here] "Enum Tags" @@ begin
          Types.Enums.generate_tags enum_definitions
        end
      end

  method pp_record_tags : PP.t GC.t =
    GC.block begin
        GC.generation_block [%here] "Record Tags" begin
          Types.Records.generate_tags record_definitions
        end
      end

  method pp_variant_tags : PP.t GC.t =
    GC.block begin
        GC.generation_block [%here] "Variant Tags" begin
            Types.Variants.generate_tags variant_definitions;
          end
      end

  method pp_base_module : PP.t GC.t =
    GC.block begin
        GC.generation_block [%here] "Base Module" begin
            BaseModule.pp_base_module all_definitions
          end
      end

  method pp_program_module : PP.t GC.t =
    let* program_module =
      ProgramModule.pp_program_module
        function_definitions
        top_level_type_constraint_definitions
    in
    GC.return @@ program_module

  method pp_finite : PP.t GC.t =
    let* finite_definitions =
      let finite_enums =
        List.map ~f:(PP.annotate [%here]) @@ Types.Enums.generate_finiteness enum_definitions
      and finite_variants =
        List.map ~f:(PP.annotate [%here]) @@ Types.Variants.generate_finiteness variant_definitions
      in
      let* finite_registers =
        GC.pp_annotate [%here] @@ Registers.pp_register_finiteness register_definitions
      in
      GC.return begin
        List.build_list @@ fun { addall; add; _ } -> begin
          addall finite_enums;
          add    finite_registers;
          addall finite_variants;
        end
      end
    in
    let parts =
      List.build_list @@
      fun { add; addall; _ } -> begin
        add    @@ PP.annotate [%here] @@ Coq.pp_imports [ "stdpp.finite" ];
        add    @@ PP.annotate [%here] @@ Coq.pp_local_obligation_tactic (PP.string "finite_from_eqdec");
        addall @@ finite_definitions;
      end
    in
    GC.block begin
      GC.generation_block [%here] "Finite" begin
        GC.return begin
          Coq.pp_section (PP.string "Finite") @@ PP.(paragraphs parts)
        end
      end
    end

  method pp_no_confusion : PP.t GC.t =
    let section_identifier : PP.t =
      PP.string "TransparentObligations"
    in
    let section_contents =
      (*
        Collect identifiers for which to declare NoConfusion
        Note: order is important
      *)
      let no_confusion_identifiers =
        let no_confusion_identifier_from_definition _sail_definition (type_definitions : Ast.Definition.Type.t) =
          match type_definitions with
          | Abbreviation _                  -> []
          | Variant      variant_definition -> Types.Variants.no_confusion_identifiers_for variant_definition
          | Enum         enum_definition    -> Types.Enums.no_confusion_identifiers_for enum_definition
          | Record       record_definition  -> Types.Records.no_confusion_identifiers_for record_definition
        in
        let no_confusions_from_definitions =
          List.concat_map ~f:(Fn.uncurry no_confusion_identifier_from_definition) type_definitions
        in
        List.concat [
          no_confusions_from_definitions;
          Types.Enums.extra_no_confusion_identifiers ();
          Types.Variants.extra_no_confusion_identifiers ();
          Types.Records.extra_no_confusion_identifiers ();
          Registers.extra_no_confusion_identifiers ();
        ]
      in
      let transparent_obligations =
        PP.annotate [%here] @@ PP.string "Local Set Transparent Obligations."
      and no_confusion_lines =
        PP.annotate [%here] @@ PP.vertical @@ List.map ~f:Coq.pp_derive_no_confusion_for (List.map ~f:Identifier.pp no_confusion_identifiers)
      in
      PP.annotate [%here] @@ PP.paragraphs [ transparent_obligations; no_confusion_lines ]
    in
    GC.block begin
        GC.generation_block [%here] "No Confusion" begin
          GC.return @@ Coq.pp_section section_identifier section_contents
        end
      end

  method pp_eqdecs : PP.t GC.t =
    (*
      Collect identifiers for which to declare EqDec
      Note: order is important
      If EqDecs are in wrong order, Coq will silently fail
    *)
    let eqdec_identifiers =
      let eqdec_identifier_from_definition _sail_definition (type_definition : Ast.Definition.Type.t) =
        match type_definition with
        | Abbreviation _                  -> [ ]
        | Variant      variant_definition -> Types.Variants.eqdec_identifiers_for variant_definition
        | Enum         enum_definition    -> Types.Enums.eqdec_identifiers_for enum_definition
        | Record       record_definition  -> Types.Records.eqdec_identifiers_for record_definition
      in
      let eqdecs_from_definitions =
        List.concat_map ~f:(Fn.uncurry eqdec_identifier_from_definition) type_definitions
      in
      List.concat [
        eqdecs_from_definitions;
        Types.Enums.extra_eqdec_identifiers ();
        Types.Variants.extra_eqdec_identifiers ();
        Types.Records.extra_eqdec_identifiers ();
        Registers.extra_eqdec_identifiers ();
      ]
    in
    let coq_lines =
      List.map ~f:Coq.pp_derive_eqdec_for (List.map ~f:Identifier.pp eqdec_identifiers)
    in
    GC.block begin
      GC.generation_block [%here] "EqDec" begin
        GC.return begin
          PP.annotate [%here] @@ PP.vertical coq_lines
        end
      end
    end

  method pp_value_definitions : PP.t GC.t =
    ValueDefinitions.generate all_definitions

  method pp_base : PP.t GC.t =
    let* sections = GC.sequence [
      self#pp_base_prelude;
      self#pp_register_definitions;
      self#pp_translated_type_definitions;
      self#pp_enum_tags;
      self#pp_variant_tags;
      self#pp_record_tags;
      self#pp_no_confusion;
      self#pp_eqdecs;
      self#pp_finite;
      self#pp_base_module;
      self#pp_value_definitions;
    ]
    in
    GC.return @@ PP.(paragraphs sections)

  method pp_program : PP.t GC.t =
    let* sections = GC.sequence [
        self#pp_program_prelude;
        self#pp_program_module;
      ]
    in
    GC.return @@ PP.annotate [%here] @@ PP.(paragraphs sections)

  method pp_argument_types_of_polymorphic_function_calls : PP.t GC.t =
    let pairs : (Ast.Identifier.t * Ast.Type.t list list) list =
      Ast.Identifier.Map.to_alist intermediate_representation.polymorphic_argtypes
    in
    let* entries =
      let build_entry
          (function_identifier : Ast.Identifier.t    )
          (argument_types_list : Ast.Type.t list list) : (PP.t * PP.t) GC.t
        =
        let pp_function_identifier =
          Identifier.pp function_identifier
        in
        let* pp_argument_types_list : PP.t =
          let pp_argument_types (argument_types : Ast.Type.t list) : PP.t GC.t =
            let* pp_argument_types =
              GC.map ~f:Type.pp_nanotype argument_types
            in
            let index_formatter (index : int) : PP.t =
              PP.(horizontal [ string "arg #"; integer index; string ": " ])
            in
            GC.return begin
              PP.vertical [
                PP.string "Argument types";
                PP.indent begin
                  PP.numbered_list ~index_formatter:(Some index_formatter) pp_argument_types
                end
              ]
            end
          in
          let* pp_argument_types_list =
            GC.map argument_types_list ~f:pp_argument_types
          in
          GC.return @@ PP.numbered_list pp_argument_types_list
        in
        GC.return (
          pp_function_identifier,
          pp_argument_types_list
        )
      in
      GC.map ~f:(Fn.uncurry build_entry) pairs
    in
    GC.return @@ PP.description_list entries
end
