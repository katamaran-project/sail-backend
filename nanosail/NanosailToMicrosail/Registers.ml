open Base
open Monads.Notations.Star(AnnotationContext)
open Identifier
open Auxlib

module AC = AnnotationContext


let reg_inductive_type register_definitions =
  let identifier = PP.string "Reg"
  and typ = PP.string "Ty -> Set"
  in
  let inductive_type =
    Coq.build_inductive_type identifier typ (fun add_constructor ->
        let make_constructor (register_definition : Ast.Definition.register_definition) =
          let identifier = pp_identifier register_definition.identifier
          in
          let* register_type = Nanotype.pp_nanotype register_definition.typ
          in
          let typ = PP.(separate space [ string "Reg"; register_type  ])
          in
          add_constructor ~typ:typ identifier
        in
        AC.iter ~f:make_constructor register_definitions
      )
  in
  Coq.annotate inductive_type


let no_confusion_for_reg () =
  Coq.section (Ast.Identifier.mk "TransparentObligations") (
      PP.(separate hardline [
          string "Local Set Transparent Obligations.";
          string "Derive Signature NoConfusion NoConfusionHom EqDec for Reg."
        ])
    )


let reg_definition () =
  PP.utf8string "Definition 𝑹𝑬𝑮 : Ty -> Set := Reg."


let translate_regname (register_identifier : Ast.Identifier.t) : Ast.Identifier.t =
  let prefix = "RegName_"
  in
  Ast.Identifier.mk @@ Printf.sprintf "%s%s" prefix (Ast.Identifier.string_of register_identifier)

(*
  Defines RegName inductive type enumerating all registers

  Inductive RegName : Set :=
  | translate_regname(R1)
  | translate_regname(R2)
  | translate_regname(R3)
  | translate_regname(R4)
  .

 *)
let regname_inductive_type (register_definitions : (Sail.sail_definition * Ast.Definition.register_definition) list) =
  if List.is_empty register_definitions
  then None
  else begin
      let register_names =
        List.map ~f:(fun (_, def) -> def.identifier) register_definitions
      in
      let type_name = PP.string "RegName"
      and typ = PP.string "Set"
      in
      let inductive_type =
        Coq.build_inductive_type type_name typ (fun add_constructor ->
            AC.iter register_names ~f:(fun name -> add_constructor @@ pp_identifier @@ translate_regname name)
          )
      in
      Option.some @@ Coq.annotate inductive_type
    end

let instance_reg_eq_dec register_names =
  let cases =
    let cs =
      List.map ~f:(fun register_name ->
          let rn_str = register_name in
          ((rn_str, rn_str), PP.string "left eq_refl"))
        register_names
    and wildcard_case = PP.((string "_", string "_"), string "right _")
    in
    Auxlib.build_list (fun { add; addall; _ } ->
        addall cs;
        if List.length cs > 1
        then add wildcard_case;
      )
  in
  PP.(
    separate hardline [
      utf8string "#[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=";
      utf8string "  fun '(existT σ x) '(existT τ y) =>";
      indent' (Coq.match_pair (string "x", string "y") cases) ^^ Coq.eol;
      string "Proof. all: transparent_abstract (intros H; depelim H). Defined."
    ]
  )

let reg_finite register_names =
  let enum_values =
    let enum_value_of_register_name register_name =
      PP.(
        separate space [
          string "existT";
          underscore;
          register_name
        ]
      )
    in
    Coq.list (List.map ~f:enum_value_of_register_name register_names)
  in
  PP.(
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
  )

let obligation_tactic =
  PP.(
    separate hardline [
      string "Local Obligation Tactic :=";
      string "  finite_from_eqdec."
    ]
  )

let generate (register_definitions : (Sail.sail_definition * Ast.Definition.register_definition) list) : PP.document =
  let register_names =
    let extract_identifier (pair : Sail.sail_definition * Ast.Definition.register_definition) =
      pp_identifier (snd pair).identifier
    in
    List.map ~f:extract_identifier register_definitions
  in
  let section_contents =
    Coq.sentence @@ PP.(separate (twice hardline) [
      reg_inductive_type @@ List.map ~f:snd register_definitions;
      no_confusion_for_reg ();
      reg_definition ();
      instance_reg_eq_dec register_names;
      obligation_tactic;
      reg_finite register_names
    ])
  in
  Coq.section (Ast.Identifier.mk "RegDeclKit") section_contents


let generate_noconfusions (register_definitions : (Sail.sail_definition * Ast.Definition.register_definition) list) =
  let has_registers =
    not @@ List.is_empty register_definitions
  in
  let contents =
    build_list (fun { add; _ } ->
        add @@ PP.string "Local Set Transparent Obligations.";
        add @@ PP.string "";
        if has_registers
        then add @@ PP.string "Derive NoConfusion for RegName.";
      )
  in
  let section =
    Coq.section (Ast.Identifier.mk "TransparentObligations") PP.(separate hardline contents)
  in
  Some section
