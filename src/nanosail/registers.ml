open PPrint
open Ast

module PP = PPrint
module PU = Pputil
module Coq = Coq_generation

module S = struct
  include Sail_util
end


let reg_inductive_type register_definitions =
  let identifier = string "Reg"
  and typ = string "Ty -> Set"
  and constructors =
    let constructor_of_register register_definition =
      let identifier = string register_definition.identifier
      and typ = string "Reg" ^^ space ^^ S.pp_ty register_definition.typ
      in
      (identifier, typ)
    in
    List.map constructor_of_register register_definitions
  in
  Coq.inductive_type identifier typ constructors

let no_confusion_for_reg () =
  Coq.section "TransparentObligations" (
      separate hardline [
          string "Local Set Transparent Obligations.";
          string "Derive Signature NoConfusion (* NoConfusionHom *) for Reg."
        ]
    )

let reg_definition () =
  utf8string "Definition 𝑹𝑬𝑮 : Ty -> Set := Reg."

let instance_reg_eq_dec register_names =
  let cases =
    let cs =
      List.map (fun register_name ->
          let rn_str = register_name in
          ((rn_str, rn_str), string "left eq_refl"))
        register_names
    and wildcard_case = ((string "_", string "_"), string "right _")
    in
    Util.list_builder (fun { add; addall } ->
        addall cs;
        add wildcard_case
      )
  in
  separate hardline [
      utf8string "#[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=";
      utf8string "  fun '(existT σ x) '(existT τ y) =>";
      PU.indent' (Coq.match_pair (string "x", string "y") cases) ^^ Coq.eol;
      string "Proof. all: transparent_abstract (intros H; depelim H). Defined."
    ]

let reg_finite register_names =
  let enum_values =
    let enum_value_of_register_name register_name =
      separate space [
        string "existT";
        underscore;
        register_name
      ]
    in
    Coq.list (List.map enum_value_of_register_name register_names)
  in
  separate hardline (
    [
      utf8string "Program Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :=";
      PU.indent (
        Coq.record_value [
          (string "enum", enum_values)
        ]
      )
    ]
  )


let pp_register_module (register_definitions : (sail_definition * register_definition) list) : document =
  let register_names =
    List.map (fun (_, def) -> string def.identifier) register_definitions
  in
  let section_contents =
    separate (twice hardline) [
        reg_inductive_type (List.map snd register_definitions);
        no_confusion_for_reg ();
        reg_definition ();
        instance_reg_eq_dec register_names;
        reg_finite register_names
      ] ^^ Coq.eol
  in
  Coq.section "RegDeclKit" section_contents
