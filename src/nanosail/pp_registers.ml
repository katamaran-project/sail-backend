open PPrint
open Ast


module PP = struct
  include PPrint
  include Pp_util

  module Coq = Pp_coq
end


module S = struct
  include Sail_util
end


let reg_inductive_type register_definitions =
  let identifier = string "Reg"
  and typ = string "Ty -> Set"
  in
  PP.Coq.build_inductive_type identifier typ (fun add_constructor ->
      let make_constructor (register_definition : register_definition) =
        let identifier = string register_definition.identifier
        and typ = separate space [ string "Reg"; S.pp_ty register_definition.typ ]
        in
        add_constructor ~typ:typ identifier
      in
      List.iter make_constructor register_definitions
    )

let no_confusion_for_reg () =
  PP.Coq.section "TransparentObligations" (
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
    Util.build_list (fun { add; addall } ->
        addall cs;
        add wildcard_case
      )
  in
  separate hardline [
      utf8string "#[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=";
      utf8string "  fun '(existT σ x) '(existT τ y) =>";
      PP.indent' (PP.Coq.match_pair (string "x", string "y") cases) ^^ PP.Coq.eol;
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
    PP.Coq.list (List.map enum_value_of_register_name register_names)
  in
  separate hardline (
    [
      utf8string "Program Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :=";
      PP.indent (
        PP.Coq.record_value [
          (string "enum", enum_values)
        ]
      )
    ]
  )

let obligation_tactic =
  separate hardline [
    string "Local Obligation Tactic :=";
    string "  finite_from_eqdec."
    ]


let generate (register_definitions : (sail_definition * register_definition) list) : document =
  let register_names =
    let extract_identifier (pair : sail_definition * register_definition) =
      string (snd pair).identifier
    in
    List.map extract_identifier register_definitions
  in
  let section_contents =
    separate (twice hardline) [
        reg_inductive_type (List.map snd register_definitions);
        no_confusion_for_reg ();
        reg_definition ();
        instance_reg_eq_dec register_names;
        obligation_tactic;
        reg_finite register_names
      ] ^^ PP.Coq.eol
  in
  PP.Coq.section "RegDeclKit" section_contents
