open Base
open PPrint
open Ast
open Auxlib
open Util
open Monads.Notations.Star(AnnotationContext)
open FunDefKit

module AC = AnnotationContext


let defaultBase = string "Import DefaultBase."


(******************************************************************************)
(* ForeignDefKit pretty printing *)

let pp_foreignKit =
  let title = "ForeignKit"
  and contents =
    separate_map hardline utf8string [
      "Definition Memory : Set := unit.";
      "Definition ForeignCall {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs)";
      "  (res : string + Val σ) (γ γ' : RegStore) (μ μ' : Memory) : Prop := False.";
      "Lemma ForeignProgress {σs σ} (f : 𝑭𝑿 σs σ) (args : NamedEnv Val σs) γ μ :";
      "  exists γ' μ' res, ForeignCall f args res γ γ' μ μ'.";
      "Proof. destruct f. Qed."
    ]
  in
  Coq.section title contents


(******************************************************************************)
(* Program pretty printing *)

let pp_program_module
      program_name
      base_name
      function_definitions
      top_level_type_constraint_definitions =
  let flag            = Coq.Import
  and identifier      = program_name ^ "Program"
  and base_identifier = base_name ^ "Base" in
  let includes        = [ "Program"; base_identifier ]
  and contents =
    separate (twice hardline) [
      FunDeclKit.generate @@ List.map ~f:snd function_definitions;
      Coq.line @@ string @@ "Include FunDeclMixin " ^ base_identifier;
      pp_function_definition_kit function_definitions top_level_type_constraint_definitions;
      Coq.line @@ string @@"Include DefaultRegStoreKit " ^ base_identifier;
      pp_foreignKit;
      Coq.line @@ string @@ "Include ProgramMixin " ^ base_identifier;
    ]
  in
  Coq.module'
    ~flag:flag
    ~includes:includes
    identifier
    contents



(******************************************************************************)
(* Full pretty printing *)

let pp_module_header title =
  string (Printf.sprintf "(*** %s ***)" title)

let _generate_module_header title =
  AC.return @@ string @@ Printf.sprintf "(*** %s ***)" title

let fromIR_pp ir =
  let prelude =
    Prelude.generate ()
  in
  let generate_section title contents =
    string (Printf.sprintf "(*** %s ***)" title) ^^ twice hardline ^^ contents
  in
  let base =
    let translated_type_definitions =
      let type_definitions = select Extract.type_definition ir.definitions
      in
      List.map ~f:(uncurry Types.pp_type_definition) type_definitions
    in
    let extra_enum_definitions =
      let enum_definitions = select Extract.enum_definition ir.definitions
      in
      if List.is_empty enum_definitions
      then []
      else [
        Types.Enums.generate_enum_of_enums enum_definitions;
        Types.Enums.generate_eqdecs enum_definitions;
        Types.Enums.generate_no_confusions enum_definitions;
      ]
    in
    let segments =
      build_list (fun { add; addall } ->
          add    @@ pp_module_header "TYPES";
          add    @@ defaultBase;
          addall @@ translated_type_definitions;
          addall @@ extra_enum_definitions;
        )
    in
    separate small_step segments
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
      empty
    else
      generate_section "REGISTERS" @@ Registers.generate @@ select Extract.register_definition ir.definitions
  in
  let untranslated =
    if
      Configuration.(get include_untranslated_definitions)
    then
      generate_section "UNTRANSLATED" @@ Untranslated.generate @@ select Extract.untranslated_definition ir.definitions
    else
      empty
  in
  let ignored =
    if
      Configuration.(get include_ignored_definitions)
    then
      generate_section "IGNORED" @@ Ignored.generate @@ List.map ~f:fst @@ select Extract.ignored_definition ir.definitions
    else
      empty
  in
  let sections =
    [
      prelude;
      base;
      program;
      registers;
      untranslated;
      ignored
    ]
  in
  Util.separate_nonempty big_step sections


let pretty_print len out doc = ToChannel.pretty 1. len out (doc ^^ small_step)
