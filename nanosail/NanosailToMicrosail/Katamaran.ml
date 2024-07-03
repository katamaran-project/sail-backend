open Base
open Auxlib
open Monads.Notations.Star(AnnotationContext)
open FunDefKit
open ForeignKit

module AC = AnnotationContext


let pp_program_module
      program_name
      base_name
      function_definitions
      (top_level_type_constraint_definitions : (Sail.sail_definition * Ast.Definition.top_level_type_constraint_definition) list)=
  let flag            = Coq.Import
  and identifier      = program_name ^ "Program"
  and base_identifier = base_name ^ "Base" in
  let includes        = [ "Program"; base_identifier ]
  and contents =
    PP.(separate (twice hardline) [
      FunDeclKit.generate @@ List.map ~f:snd function_definitions;
      Coq.sentence @@ string @@ "Include FunDeclMixin " ^ base_identifier;
      pp_function_definition_kit function_definitions top_level_type_constraint_definitions;
      Coq.sentence @@ string @@"Include DefaultRegStoreKit " ^ base_identifier;
      pp_foreign_kit;
      Coq.sentence @@ string @@ "Include ProgramMixin " ^ base_identifier;
    ])
  in
  Coq.module'
    ~flag:flag
    ~includes:includes
    identifier
    contents


let pretty_print ir =
  let type_definitions     = Ast.(select Extract.(type_definition of_anything) ir.definitions)
  and enum_definitions     = Ast.(select Extract.(type_definition of_enum) ir.definitions)
  and record_definitions   = Ast.(select Extract.(type_definition of_record) ir.definitions)
  and variant_definitions  = Ast.(select Extract.(type_definition of_variant) ir.definitions)
  and register_definitions = Ast.(select Extract.register_definition ir.definitions)
  in

  let prelude =
    Prelude.generate ()
  in

  let generate_section title contents =
    PP.(string (Printf.sprintf "(*** %s ***)" title) ^^ twice hardline ^^ contents)
  in

  let translated_type_definitions =
    List.map ~f:(uncurry Types.pp_type_definition) type_definitions
  in

  let pp_register_definitions =
    Registers.regname_inductive_type register_definitions;
  in

  let extra_variant_definitions =
    [
      Types.Variants.generate_tags variant_definitions;
    ]
  in

  let base_module =
    BaseModule.pp_base_module ir.definitions
  in

  let program =
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

  let registers =
    if
      List.is_empty register_definitions
    then
      PP.empty
    else
      generate_section "REGISTERS" @@ Registers.generate register_definitions
  in

  let finite =
    Finite.generate ir.definitions
  in

  let no_confusion =
    let has_enums    = not @@ List.is_empty enum_definitions
    and has_variants = not @@ List.is_empty variant_definitions
    and has_records  = not @@ List.is_empty record_definitions
    in
    if
      has_enums || has_variants || has_records
    then
      let contents =
        Coq.build_lines begin fun { line; lines; empty_line; comment } ->
          line  @@ PP.string "Local Set Transparent Obligations.";
          if has_enums
          then begin
            empty_line ();
            comment @@ PP.string "NoConfusion for each enum type";
            lines @@ Types.Enums.generate_no_confusions enum_definitions;
          end;
          if has_variants
          then begin
            empty_line ();
            comment @@ PP.string "NoConfusion for each variant/union type";
            lines @@ Types.Variants.generate_no_confusions variant_definitions;
          end;
          if has_records
          then begin
            empty_line ();
            comment @@ PP.string "NoConfusion for each record type";
            lines @@ Types.Records.generate_no_confusions record_definitions;
          end
        end
      in
      Coq.section (Ast.Identifier.mk "TransparentObligations") contents
    else
      PP.empty
  in

  let eqdecs =
    let has_enums       = not @@ List.is_empty enum_definitions
    and has_variants    = not @@ List.is_empty variant_definitions
    and has_records     = not @@ List.is_empty record_definitions
    and need_separation = ref false 
    in
    Coq.build_lines begin fun { lines; empty_line; comment; _ } ->
      if has_enums
      then begin
        comment @@ PP.string "EqDeq for each enum type";
        lines @@ Types.Enums.generate_eqdecs enum_definitions;
        need_separation := true;
      end;
      if has_variants
      then begin
        if !need_separation
        then empty_line ();
        comment @@ PP.string "EqDeq for each variant/union type";
        lines @@ Types.Variants.generate_eqdecs variant_definitions;
        need_separation := true;
      end;
      if has_records
      then begin
        if !need_separation
        then empty_line ();
        comment @@ PP.string "EqDeq for each record type";
        lines @@ Types.Records.generate_eqdecs record_definitions;
        need_separation := true;
      end
    end
    (* EqDec.generate ir.definitions *)
  in

  let value_definitions =
    ValueDefinitions.generate ir.definitions
  in

  let sections =
    build_list @@
      fun { add; addopt; addall } -> begin
          (* add    @@ prelude; *)
          (* add    @@ PP.string "Import DefaultBase."; *)
          (* addopt @@ pp_register_definitions; *)
          (* addall @@ translated_type_definitions; *)
          (* add    @@ Types.Enums.generate_tags enum_definitions; *)
          (* addall @@ extra_variant_definitions; *)
          (* add    @@ Types.Records.generate_tags record_definitions; *)
          (* addopt @@ Registers.generate_noconfusions register_definitions; *)
          (* add    @@ no_confusion; *)
          (* add    @@ eqdecs; *)
          (* addopt @@ finite; *)
          add    @@ base_module;
          (* add    @@ value_definitions; *)
          (* add    @@ program; *)
          (* add    @@ registers; *)
        end
  in
  PP.(separate_nonempty small_step sections)


let output_document_to_channel len out doc =
  PP.(ToChannel.pretty 1. len out (doc ^^ small_step))
