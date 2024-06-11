open Base
open Ast
open Auxlib
open Monads.Notations.Star(AnnotationContext)
open FunDefKit
open ForeignKit

module AC = AnnotationContext


let defaultBase = PP.string "Import DefaultBase."


(******************************************************************************)
(* Program pretty printing *)

let pp_program_module
      program_name
      base_name
      function_definitions
      (top_level_type_constraint_definitions : (Sail.sail_definition * top_level_type_constraint_definition) list)=
  let flag            = Coq.Import
  and identifier      = program_name ^ "Program"
  and base_identifier = base_name ^ "Base" in
  let includes        = [ "Program"; base_identifier ]
  and contents =
    PP.(separate (twice hardline) [
      FunDeclKit.generate @@ List.map ~f:snd function_definitions;
      Coq.line @@ string @@ "Include FunDeclMixin " ^ base_identifier;
      pp_function_definition_kit function_definitions top_level_type_constraint_definitions;
      Coq.line @@ string @@"Include DefaultRegStoreKit " ^ base_identifier;
      pp_foreign_kit;
      Coq.line @@ string @@ "Include ProgramMixin " ^ base_identifier;
    ])
  in
  Coq.module'
    ~flag:flag
    ~includes:includes
    identifier
    contents



(******************************************************************************)
(* Full pretty printing *)

let pp_module_header title =
  PP.string (Printf.sprintf "(*** %s ***)" title)

let _generate_module_header title =
  AC.return @@ PP.string @@ Printf.sprintf "(*** %s ***)" title

let pretty_print ir =
  let prelude =
    Prelude.generate ()
  in

  let generate_section title contents =
    PP.(string (Printf.sprintf "(*** %s ***)" title) ^^ twice hardline ^^ contents)

  in

  let base =
    let translated_type_definitions =
      let type_definitions = select Extract.(type_definition of_anything) ir.definitions
      in
      List.map ~f:(uncurry Types.pp_type_definition) type_definitions
    in
    let extra_enum_definitions =
      let enum_definitions = select Extract.(type_definition of_enum) ir.definitions
      in
      if List.is_empty enum_definitions
      then []
      else [
          Types.Enums.generate_enum_of_enums enum_definitions;
          Types.Enums.generate_no_confusions enum_definitions;
          Types.Enums.generate_eqdecs enum_definitions;
        ]
    in
    let extra_union_definitions =
      Coq.annotate @@ Coq.build_inductive_type
        (PP.string "Unions")
        (PP.string "Set")
        (fun _add_constructor -> AC.return ())
    in
    let extra_record_definitions =
      Coq.annotate @@ Coq.build_inductive_type
        (PP.string "Records")
        (PP.string "Set")
        (fun _add_constructor -> AC.return ())
    in
    let base_module =
      let base_module_name = "UntitledBase"
      in
      PP.(separate hardline [
              string "Module Export " ^^ string base_module_name ^^ string " <: Base.";
              string "#[export] Instance typedeclkit : TypeDeclKit :=";
              string "  {| enumi   := Enums;";
              string "     unioni  := Unions;";
              string "     recordi := Records;";
              string "  |}.";
              string "End " ^^ string base_module_name ^^ dot;
      ])
    in
    let segments =
      build_list (fun { add; addall; addopt } ->
          add    @@ pp_module_header "TYPES";
          add    @@ defaultBase;
          addopt @@ Registers.regnames @@ select Extract.register_definition ir.definitions;
          addall @@ translated_type_definitions;
          addall @@ extra_enum_definitions;
          add    @@ extra_union_definitions;
          add    @@ extra_record_definitions;
          add    @@ base_module;
        )
    in
    PP.(separate small_step segments)
  in
  
  let program =
    generate_section
      "PROGRAM"
      (
        pp_program_module
          ir.program_name
          "Default"
          (select Extract.function_definition ir.definitions)
          (select Extract.top_level_type_constraint_definition ir.definitions)
      )
  in
  
  let registers =
    if
      List.is_empty @@ select Extract.register_definition ir.definitions
    then
      PP.empty
    else
      let register_definitions = select Extract.register_definition ir.definitions
      in
      generate_section "REGISTERS" @@ Registers.generate register_definitions
  in
  
  let no_confusion =
    NoConfusion.generate ir.definitions
  in
  
  let finite =
    Finite.generate ir.definitions
  in
  
  let eqdecs =
    EqDec.generate ir.definitions
  in
  
  let value_definitions =
    ValueDefinitions.generate ir.definitions
  in
  
  let sections =
    build_list @@
      fun { add; addopt; _ } -> begin
          add    prelude;
          add    base;
          add    value_definitions;
          add    program;
          add    registers;
          addopt no_confusion;
          addopt eqdecs;
          addopt finite;
        end
  in
  PP.(separate_nonempty big_step sections)


let output_document_to_channel len out doc =
  PP.(ToChannel.pretty 1. len out (doc ^^ small_step))
