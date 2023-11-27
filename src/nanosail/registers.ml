open PPrint
open Ast

module PP = PPrint
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
    List.map (fun register_name ->
        let rn_str = string register_name in
        ((rn_str, rn_str), string "left eq_refl"))
      register_names
  in
  separate hardline [
      utf8string "#[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=";
      utf8string "  fun '(existT σ x) '(existT τ y) =>";
      Pputil.indent' (Coq.match_pair (string "x", string "y") cases);
      string "Proof. all: transparent_abstract (intros H; depelim H). Defined."
    ]

let pp_register_module (register_definitions : (sail_definition * register_definition) list) : document =
  let section_contents =
    separate (twice hardline) [
        reg_inductive_type (List.map snd register_definitions);
        no_confusion_for_reg ();
        reg_definition ();
        instance_reg_eq_dec (List.map (fun (_, def) -> def.identifier) register_definitions)
      ]
  in
  Coq.section "RegDeclKit" section_contents
