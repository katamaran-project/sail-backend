open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let block loc label contents =
  Coq.generation_block loc (PP.string label) contents


let pp_program_module
      (program_name                          : string                                                                           )
      (base_name                             : string                                                                           )
      (function_definitions                  : (Sail.sail_definition * Ast.Definition.Function.t) list                          )
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.top_level_type_constraint_definition) list)
  =
  let flag            = Coq.Import
  and identifier      = program_name ^ "Program"
  and base_identifier = base_name ^ "Base" in
  let includes        = [ "Program"; base_identifier ]
  and contents =
    PP.(separate (twice hardline) [
      FunDeclKit.generate @@ List.map ~f:snd function_definitions;
      Coq.sentence @@ string @@ "Include FunDeclMixin " ^ base_identifier;
      FunDefKit.pp_function_definition_kit function_definitions top_level_type_constraint_definitions;
      Coq.sentence @@ string @@"Include DefaultRegStoreKit " ^ base_identifier;
      ForeignKit.pp_foreign_kit;
      Coq.sentence @@ string @@ "Include ProgramMixin " ^ base_identifier;
    ])
  in
  Coq.module'
    ~flag:flag
    ~includes:includes
    identifier
    contents


let pretty_print (ir : Ast.program) =
  let type_definitions     = Ast.(select Extract.(type_definition of_anything) ir.definitions)
  and enum_definitions     = Ast.(select Extract.(type_definition of_enum    ) ir.definitions)
  and record_definitions   = Ast.(select Extract.(type_definition of_record  ) ir.definitions)
  and variant_definitions  = Ast.(select Extract.(type_definition of_variant ) ir.definitions)
  and register_definitions = Ast.(select Extract.register_definition           ir.definitions)
  in

  let pp_prelude =
    block [%here] "Prelude" begin
      Prelude.generate_base_prelude ()
    end
  in

  let generate_section title contents =
    PP.(string (Printf.sprintf "(*** %s ***)" title) ^^ twice hardline ^^ contents)
  in

  let pp_translated_type_definitions =
    block [%here] "Translated Type Definitions" begin
      PP.separate_map (PP.twice PP.hardline) (Auxlib.uncurry Types.pp_type_definition) type_definitions
    end
  in

  let pp_register_definitions =
    block [%here] "Register Definitions" begin
      Registers.regname_inductive_type register_definitions
    end
  in

  let pp_enum_tags =
    block [%here] "Enum Tags" begin
      Types.Enums.generate_tags enum_definitions
    end
  in

  let pp_record_tags =
    block [%here] "Record Tags" begin
      Types.Records.generate_tags record_definitions
    end
  in

  let pp_variant_tags =
    block [%here] "Variant Tags" begin
      Types.Variants.generate_tags variant_definitions;
    end
  in

  let pp_base_module =
    block [%here] "Base Module" begin
      BaseModule.pp_base_module ir.definitions
    end
  in

  let pp_program =
    generate_section
      "PROGRAM"
      (
        pp_program_module
          ir.program_name
          "Default"
          Ast.(select Extract.function_definition ir.definitions)
          Ast.(select Extract.top_level_type_constraint_definition ir.definitions)
      )
  in

  let pp_finite =
    let finite_definitions =
      let finite_enums =
        Types.Enums.generate_finiteness enum_definitions
      and finite_variants =
        Types.Variants.generate_finiteness variant_definitions
      and finite_registers =
        Registers.generate_register_finiteness register_definitions
      in
      Auxlib.build_list @@ fun { addall; add; _ } -> begin
        addall finite_enums;
        add    finite_registers;
        addall finite_variants;
      end
    in
    let parts =
      Auxlib.build_list @@
      fun { add; addall; _ } -> begin
        add    @@ Coq.imports [ "stdpp.finite" ];
        add    @@ Coq.local_obligation_tactic (Ast.Identifier.mk "finite_from_eqdec");
        addall @@ finite_definitions;
      end
    in
    block [%here] "Finite" begin
      Coq.section (Ast.Identifier.mk "Finite") @@ PP.(separate (twice hardline) parts)
    end
  in

  let pp_no_confusion =
    let section_identifier = Ast.Identifier.mk "TransparentObligations"
    in
    let section_contents =
      let eqdec_identifiers = List.concat [
          Types.Enums.required_no_confusions enum_definitions;
          Types.Variants.required_no_confusions variant_definitions;
          Types.Records.required_no_confusions record_definitions;
          Registers.required_no_confusions register_definitions;
        ]
      in
      let transparent_obligations =
        PP.string "Local Set Transparent Obligations."
      and no_confusion_lines =
        PP.separate_map PP.hardline Coq.derive_no_confusion_for eqdec_identifiers
      in
      PP.separate (PP.twice PP.hardline) [ transparent_obligations; no_confusion_lines ]
    in
    block [%here] "No Confusion" begin
      Coq.section section_identifier section_contents
    end
  in

  let pp_eqdecs =
    let eqdec_identifiers = List.concat [
        Types.Enums.required_eqdecs enum_definitions;
        Types.Variants.required_eqdecs variant_definitions;
        Types.Records.required_eqdecs record_definitions;
        Registers.required_eqdecs register_definitions;
      ]
    in
    let coq_lines = List.map ~f:Coq.derive_eqdec_for eqdec_identifiers
    in
    block [%here] "EqDec" begin
      PP.separate PP.hardline coq_lines
    end
  in

  let pp_value_definitions =
    ValueDefinitions.generate ir.definitions
  in

  let _ = pp_program in (* remove this *)
  
  let sections =
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
  PP.(separate_nonempty small_step sections)


let output_document_to_channel len out doc =
  PP.(ToChannel.pretty 1. len out (doc ^^ small_step))
