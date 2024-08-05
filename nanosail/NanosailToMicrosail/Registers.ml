open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module GC = CoqGenerationContext


(* Name for the inductive type listing all registers *)
let regname_inductive_type_identifier =
  Ast.Identifier.mk "RegName"


let regname_tag =
  Ast.Identifier.mk "regname"


let reg_inductive_type (register_definitions : Ast.Definition.register_definition list) : PP.document =
  let identifier = PP.string "Reg"
  and typ = PP.string "Ty -> Set"
  in
  let inductive_type =
    Coq.build_inductive_type identifier typ (fun add_constructor ->
        let make_constructor (register_definition : Ast.Definition.register_definition) =
          let identifier = Identifier.pp register_definition.identifier
          in
          let* register_type = Nanotype.pp_nanotype register_definition.typ
          in
          let typ = PP.(separate space [ string "Reg"; parens register_type  ])
          in
          add_constructor ~typ:typ identifier
        in
        AC.iter ~f:make_constructor register_definitions
      )
  in
  Coq.generation_block [%here] (PP.string "Reg Inductive Type") begin
    Coq.annotate inductive_type
  end


let no_confusion_for_reg () : PP.document =
  Coq.generation_block [%here] (PP.string "No Confusion for Reg") begin
    Coq.section (Ast.Identifier.mk "TransparentObligations") (
      PP.(separate hardline [
          string "Local Set Transparent Obligations.";
          string "Derive Signature NoConfusion NoConfusionHom EqDec for Reg."
        ])
    )
  end


let reg_definition () : PP.document =
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
let regname_inductive_type (register_definitions : (Sail.sail_definition * Ast.Definition.register_definition) list) : PP.document =
  let register_names =
    List.map ~f:(fun (_, def) -> def.identifier) register_definitions
  in
  let type_name = Identifier.pp regname_inductive_type_identifier
  and typ = PP.string "Set"
  in
  let inductive_type =
    Coq.build_inductive_type type_name typ (fun add_constructor ->
        AC.iter register_names ~f:(fun name -> add_constructor @@ Identifier.pp @@ translate_regname name)
      )
  in
  Coq.generation_block [%here] (PP.string "Regname Inductive Type") begin
    Coq.annotate inductive_type
  end


let instance_reg_eq_dec (register_names : PP.document list) : PP.document =
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
  let id1 = Configuration.tag_as_generated @@ Ast.Identifier.mk "x"
  and id2 = Configuration.tag_as_generated @@ Ast.Identifier.mk "y"
  in
  Coq.generation_block [%here] (PP.string "REG_eq_dec Instance") begin
    PP.(
      separate hardline [
        utf8string "#[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=";
        PP.(string "  fun '(existT σ " ^^ (Identifier.pp id1) ^^ string ") '(existT τ " ^^ (Identifier.pp id2) ^^ string ") =>");
        indent' (Coq.match_pair (Identifier.pp id1, Identifier.pp id2) cases) ^^ Coq.eol;
        string "Proof. all: transparent_abstract (intros H; depelim H). Defined."
      ]
    )
  end


let reg_finite (register_names : PP.document list) : PP.document =
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
  Coq.generation_block [%here] (PP.string "REG_finite Instance") begin
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
  end


let obligation_tactic () : PP.document =
  Coq.generation_block [%here] (PP.string "Obligation Tactic") begin
    PP.(
      separate hardline [
        string "Local Obligation Tactic :=";
        string "  finite_from_eqdec."
      ]
    )
  end

let generate_regdeclkit (register_definitions : (Sail.sail_definition * Ast.Definition.register_definition) list) : PP.document =
  let register_names =
    let extract_identifier (pair : Sail.sail_definition * Ast.Definition.register_definition) =
      Identifier.pp (snd pair).identifier
    in
    List.map ~f:extract_identifier register_definitions
  in
  let section_contents =
    Coq.sentence @@ PP.(separate (twice hardline) [
      reg_inductive_type @@ List.map ~f:snd register_definitions;
      no_confusion_for_reg ();
      reg_definition ();
      instance_reg_eq_dec register_names;
      obligation_tactic ();
      reg_finite register_names
    ])
  in
  Coq.section (Ast.Identifier.mk "RegDeclKit") section_contents


let extra_eqdec_identifiers () =
  [ regname_inductive_type_identifier ]


let extra_no_confusion_identifiers () =
  [ regname_inductive_type_identifier ]


let generate_register_finiteness (register_definitions : (Sail.sail_definition * Ast.Definition.register_definition) list) : PP.document =
  let register_identifiers =
    List.map ~f:(fun (_, def) -> def.identifier) register_definitions
  in
  let translated_register_identifiers =
    List.map ~f:translate_regname register_identifiers
  in
  let identifier = Identifier.pp @@ Ast.Identifier.mk "RegName"
  and type_name  = Identifier.pp @@ Ast.Identifier.mk "RegName"
  and values     = List.map ~f:Identifier.pp translated_register_identifiers
  in
  Coq.finite_instance ~identifier ~type_name ~values
