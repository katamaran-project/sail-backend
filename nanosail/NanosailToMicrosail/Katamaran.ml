open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


let genblock loc label contents =
  GC.generation_block loc (PP.string label) contents


class katamaran (intermediate_representation : Ast.program) = object(self : 'self)
  val program_name             = intermediate_representation.program_name
  val all_definitions          = intermediate_representation.definitions
  val type_definitions         = Ast.Definition.Select.(select (type_definition of_anything) intermediate_representation.definitions)
  val enum_definitions         = Ast.Definition.Select.(select (type_definition of_enum    ) intermediate_representation.definitions)
  val record_definitions       = Ast.Definition.Select.(select (type_definition of_record  ) intermediate_representation.definitions)
  val variant_definitions      = Ast.Definition.Select.(select (type_definition of_variant ) intermediate_representation.definitions)
  val register_definitions     = Ast.Definition.Select.(select register_definition           intermediate_representation.definitions)
  val ignored_definitions      = Ast.Definition.Select.(select ignored_definition            intermediate_representation.definitions)
  val untranslated_definitions = Ast.Definition.Select.(select untranslated_definition       intermediate_representation.definitions)

  method program_name             = program_name
  method all_definitions          = all_definitions
  method type_definitions         = type_definitions
  method enum_definitions         = enum_definitions
  method record_definitions       = record_definitions
  method variant_defintions       = variant_definitions
  method register_definitions     = register_definitions
  method ignored_definitions      = ignored_definitions
  method untranslated_definitions = untranslated_definitions
  
  method pp_base_prelude : PP.document GC.t =
    GC.generation_block [%here] (PP.string "Prelude") @@* begin
      Prelude.generate_base_prelude ()
    end

  method pp_program_prelude : PP.document GC.t =
    GC.generation_block [%here] (PP.string "Prelude") @@* begin
      Prelude.generate_program_prelude ()
    end

  method pp_register_definitions : PP.document GC.t =
    GC.generation_block [%here] (PP.string "Register Definitions") @@* begin
      Registers.pp_regname_inductive_type register_definitions
    end

  method pp_translated_type_definitions : PP.document GC.t =
    genblock [%here] "Translated Type Definitions" @@* begin
      let* type_definitions' =
        GC.map ~f:(Auxlib.uncurry Types.pp_type_definition) type_definitions
      in
      GC.return @@ PP.vertical ~separator:PP.(twice hardline) type_definitions'
    end

  method pp_enum_tags : PP.document GC.t =
    genblock [%here] "Enum Tags" @@* begin
      Types.Enums.generate_tags enum_definitions
    end

  method pp_record_tags : PP.document GC.t =
    genblock [%here] "Record Tags" @@* begin
      Types.Records.generate_tags record_definitions
    end

  method pp_variant_tags : PP.document GC.t =
    genblock [%here] "Variant Tags" @@* begin
      Types.Variants.generate_tags variant_definitions;
    end

  method pp_base_module : PP.document GC.t =
    let* base_module =
      BaseModule.pp_base_module all_definitions
    in
    GC.generation_block [%here] (PP.string "Base Module") base_module

  method pp_program : PP.document GC.t =
    let* program_module =
      ProgramModule.pp_program_module
        program_name
        "Default"
        Ast.Definition.Select.(select function_definition all_definitions)
        Ast.Definition.Select.(select top_level_type_constraint_definition all_definitions)
    in
    GC.return @@ program_module

  method pp_finite : PP.document GC.t =
    let* finite_definitions =
      let finite_enums =
        Types.Enums.generate_finiteness enum_definitions
      and finite_variants =
        Types.Variants.generate_finiteness variant_definitions
      in
      let* finite_registers =
        Registers.pp_register_finiteness register_definitions
      in
      GC.return begin
        Auxlib.build_list @@ fun { addall; add; _ } -> begin
          addall finite_enums;
          add    finite_registers;
          addall finite_variants;
        end
      end
    in
    let parts =
      Auxlib.build_list @@
      fun { add; addall; _ } -> begin
        add    @@ Coq.pp_imports [ "stdpp.finite" ];
        add    @@ Coq.pp_local_obligation_tactic (Ast.Identifier.mk "finite_from_eqdec");
        addall @@ finite_definitions;
      end
    in
    genblock [%here] "Finite" begin
      Coq.pp_section (Ast.Identifier.mk "Finite") @@ PP.(separate (twice hardline) parts)
    end

  method pp_no_confusion : PP.document GC.t =
    let section_identifier = Ast.Identifier.mk "TransparentObligations"
    in
    let section_contents =
      (*
        Collect identifiers for which to declare NoConfusion
        Note: order is important
      *)
      let no_confusion_identifiers =
        let no_confusion_identifier_from_definition _sail_definition (type_definitions : Ast.Definition.Type.t) =
          match type_definitions with
          | Abbreviation _                  -> [ ]
          | Variant      variant_definition -> Types.Variants.no_confusion_identifiers_for variant_definition
          | Enum         enum_definition    -> Types.Enums.no_confusion_identifiers_for enum_definition
          | Record       record_definition  -> Types.Records.no_confusion_identifiers_for record_definition
        in
        let no_confusions_from_definitions =
          List.concat_map ~f:(Auxlib.uncurry no_confusion_identifier_from_definition) type_definitions
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
        PP.string "Local Set Transparent Obligations."
      and no_confusion_lines =
        PP.separate_map PP.hardline Coq.pp_derive_no_confusion_for no_confusion_identifiers
      in
      PP.separate (PP.twice PP.hardline) [ transparent_obligations; no_confusion_lines ]
    in
    genblock [%here] "No Confusion" begin
      Coq.pp_section section_identifier section_contents
    end

  method pp_eqdecs : PP.document GC.t =
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
        List.concat_map ~f:(Auxlib.uncurry eqdec_identifier_from_definition) type_definitions
      in
      List.concat [
        eqdecs_from_definitions;
        Types.Enums.extra_eqdec_identifiers ();
        Types.Variants.extra_eqdec_identifiers ();
        Types.Records.extra_eqdec_identifiers ();
        Registers.extra_eqdec_identifiers ();
      ]
    in
    let coq_lines = List.map ~f:Coq.pp_derive_eqdec_for eqdec_identifiers
    in
    genblock [%here] "EqDec" begin
      PP.separate PP.hardline coq_lines
    end

  method pp_value_definitions : PP.document GC.t =
    ValueDefinitions.generate all_definitions

  method pp_base : PP.document GC.t =
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
    GC.return @@ PP.(separate_nonempty (twice hardline) sections)
end


let pretty_print (ir : Ast.program) : PP.document GC.t =
  let katamaran = new katamaran ir
  in
  let* sections = GC.sequence
    [
      katamaran#pp_program;
    ]
  in
  GC.return @@ PP.(separate_nonempty (twice hardline) sections)


let full_translation (ir : Ast.program) : PP.document =
  GC.generate @@ pretty_print ir
