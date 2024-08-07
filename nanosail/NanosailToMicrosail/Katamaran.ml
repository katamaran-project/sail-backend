open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let genblock loc label contents =
  GC.generation_block loc (PP.string label) contents


let pretty_print (ir : Ast.program) : PP.document GC.t =
  let type_definitions     = Ast.Definition.Select.(select (type_definition of_anything) ir.definitions)
  and enum_definitions     = Ast.Definition.Select.(select (type_definition of_enum    ) ir.definitions)
  and record_definitions   = Ast.Definition.Select.(select (type_definition of_record  ) ir.definitions)
  and variant_definitions  = Ast.Definition.Select.(select (type_definition of_variant ) ir.definitions)
  and register_definitions = Ast.Definition.Select.(select register_definition           ir.definitions)
  in

  let pp_prelude : PP.document GC.t =
    GC.generation_block [%here] (PP.string "Prelude") @@* begin
      Prelude.generate_base_prelude ()
    end
  in

  let pp_register_definitions : PP.document GC.t =
    GC.generation_block [%here] (PP.string "Register Definitions") @@* begin
      Registers.pp_regname_inductive_type register_definitions
    end
  in

  let generate_section title contents =
    PP.(string (Printf.sprintf "(*** %s ***)" title) ^^ twice hardline ^^ contents)
  in

  let pp_translated_type_definitions : PP.document GC.t =
    genblock [%here] "Translated Type Definitions" @@* begin
      let* type_definitions' =
        GC.map ~f:(Auxlib.uncurry Types.pp_type_definition) type_definitions
      in
      GC.return @@ PP.vertical ~separator:PP.(twice hardline) type_definitions'
    end
  in

  let pp_enum_tags : PP.document GC.t =
    genblock [%here] "Enum Tags" @@* begin
      Types.Enums.generate_tags enum_definitions
    end
  in

  let pp_record_tags : PP.document GC.t =
    genblock [%here] "Record Tags" @@* begin
      Types.Records.generate_tags record_definitions
    end
  in

  let pp_variant_tags : PP.document GC.t =
    genblock [%here] "Variant Tags" @@* begin
      Types.Variants.generate_tags variant_definitions;
    end
  in

  let pp_base_module : PP.document GC.t =
    let* base_module =
      BaseModule.pp_base_module ir.definitions
    in
    GC.generation_block [%here] (PP.string "Base Module") base_module
  in

  let pp_program : PP.document GC.t =
    let* program_module =
      ProgramModule.pp_program_module
        ir.program_name
        "Default"
        Ast.Definition.Select.(select function_definition ir.definitions)
        Ast.Definition.Select.(select top_level_type_constraint_definition ir.definitions)
    in
    GC.return @@ generate_section "PROGRAM" program_module
  in

  let pp_finite : PP.document GC.t =
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
  in

  let pp_no_confusion : PP.document GC.t =
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
  in

  let pp_eqdecs =
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
  in

  let pp_value_definitions : PP.document GC.t =
    ValueDefinitions.generate ir.definitions
  in

  let _ = pp_program in (* remove this *)

  let* sections = GC.sequence
    [
      pp_prelude;
      pp_register_definitions;
      pp_translated_type_definitions;
      pp_enum_tags;
      pp_variant_tags;
      pp_record_tags;
      pp_no_confusion;
      pp_eqdecs;
      pp_finite;
      pp_base_module;
      pp_value_definitions;
      (* pp_program; *)
    ]
  in
  GC.return @@ PP.(separate_nonempty (twice hardline) sections)


let full_translation (ir : Ast.program) : PP.document =
  GC.generate @@ pretty_print ir
