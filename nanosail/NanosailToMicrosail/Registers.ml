open Base
open PPrint
open Ast
open Monads.Notations.Star(AnnotationContext)
open Identifier

module AC = AnnotationContext


let reg_inductive_type register_definitions =
  let identifier = string "Reg"
  and typ = string "Ty -> Set"
  in
  let inductive_type =
    Coq.mbuild_inductive_type identifier typ (fun add_constructor ->
        let make_constructor (register_definition : register_definition) =
          let identifier = pp_identifier register_definition.identifier
          in
          let* register_type = Nanotype.pp_nanotype register_definition.typ
          in
          let typ = separate space [ string "Reg"; register_type  ]
          in
          add_constructor ~typ:typ identifier
        in
        AC.iter ~f:make_constructor register_definitions
      )
  in
  Coq.annotate inductive_type

let no_confusion_for_reg () =
  Coq.section (Id.mk "TransparentObligations") (
      separate hardline [
          string "Local Set Transparent Obligations.";
          string "Derive Signature NoConfusion (* NoConfusionHom *) for Reg."
        ]
    )

let reg_definition () =
  utf8string "Definition 𝑹𝑬𝑮 : Ty -> Set := Reg."

(*
  Defines RegName inductive type enumerating all registers

  Inductive RegName : Set :=
  | R1
  | R2
  | R3
  | R4
.

 *)
let regnames (register_definitions : (sail_definition * register_definition) list) =
  if List.is_empty register_definitions
  then None
  else begin
      let register_names =
        List.map ~f:(fun (_, def) -> def.identifier) register_definitions
      in
      let type_name = string "RegName"
      and typ = string "Set"
      in
      let inductive_type = Coq.mbuild_inductive_type type_name typ (fun add_constructor ->
                               AC.iter register_names ~f:(fun name -> add_constructor @@ pp_identifier name)
                             )
      in
      Option.some @@ Coq.annotate inductive_type
    end

let instance_reg_eq_dec register_names =
  let cases =
    let cs =
      List.map ~f:(fun register_name ->
          let rn_str = register_name in
          ((rn_str, rn_str), string "left eq_refl"))
        register_names
    and wildcard_case = ((string "_", string "_"), string "right _")
    in
    Auxlib.build_list (fun { add; addall; _ } ->
        addall cs;
        add wildcard_case
      )
  in
  separate hardline [
      utf8string "#[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=";
      utf8string "  fun '(existT σ x) '(existT τ y) =>";
      PP.indent' (Coq.match_pair (string "x", string "y") cases) ^^ Coq.eol;
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
    Coq.list (List.map ~f:enum_value_of_register_name register_names)
  in
  separate hardline (
    [
      utf8string "Program Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :=";
      PP.indent' (
        Coq.record_value [
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
      pp_identifier (snd pair).identifier
    in
    List.map ~f:extract_identifier register_definitions
  in
  let section_contents =
    Coq.line @@ separate (twice hardline) [
      reg_inductive_type @@ List.map ~f:snd register_definitions;
      no_confusion_for_reg ();
      reg_definition ();
      instance_reg_eq_dec register_names;
      obligation_tactic;
      reg_finite register_names
    ]
  in
  Coq.section (Id.mk "RegDeclKit") section_contents
