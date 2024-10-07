open Base
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(* Name for the inductive type listing all registers *)
let regname_inductive_type_identifier =
  Ast.Identifier.mk "RegName"


let regname_tag =
  Ast.Identifier.mk "regname"


let pp_reg_inductive_type (register_definitions : Ast.Definition.Register.t list) : PP.document GC.t =
  let identifier = PP.string "Reg"
  and typ = PP.string "Ty -> Set"
  in
  let* inductive_type =
    GC.block begin
      GC.pp_inductive_type identifier typ (fun add_constructor ->
          let make_constructor (register_definition : Ast.Definition.Register.t) : unit GC.t =
            let identifier = Identifier.pp register_definition.identifier
            in
            let* register_type = Nanotype.pp_nanotype register_definition.typ
            in
            let typ =
              Coq.pp_application (PP.string "Reg") [ PP.(surround parens) register_type ]
            in
            add_constructor ~typ:typ identifier
          in
          GC.iter ~f:make_constructor register_definitions
        )
    end
  in
  GC.generation_block [%here] (PP.string "Reg Inductive Type") inductive_type


let pp_no_confusion_for_reg () : PP.document GC.t =
  GC.generation_block [%here] (PP.string "No Confusion for Reg") begin
    Coq.pp_section (Ast.Identifier.mk "TransparentObligations") (
      PP.(vertical [
          string "Local Set Transparent Obligations.";
          string "Derive Signature NoConfusion NoConfusionHom EqDec for Reg."
        ])
    )
  end


let pp_reg_definition () : PP.document GC.t =
  GC.return @@ PP.string "Definition 𝑹𝑬𝑮 : Ty -> Set := Reg."


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
let pp_regname_inductive_type (register_definitions : (Sail.sail_definition * Ast.Definition.Register.t) list) : PP.document GC.t =
  let register_names =
    List.map ~f:(fun (_, def) -> def.identifier) register_definitions
  in
  let type_name = Identifier.pp regname_inductive_type_identifier
  and typ = PP.string "Set"
  in
  let* inductive_type =
    GC.block begin
      GC.pp_inductive_type type_name typ (fun add_constructor ->
          GC.iter register_names ~f:(fun name -> add_constructor @@ Identifier.pp @@ translate_regname name)
        )
    end
  in
  GC.generation_block [%here] (PP.string "Regname Inductive Type") inductive_type


let pp_instance_reg_eq_dec (register_names : PP.document list) : PP.document GC.t =
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
  let pp_id1 = Identifier.pp id1
  and pp_id2 = Identifier.pp id2
  in
  GC.generation_block [%here] (PP.string "REG_eq_dec Instance") begin
    PP.(
      vertical [
        string "#[export,refine] Instance 𝑹𝑬𝑮_eq_dec : EqDec (sigT Reg) :=";
        horizontal [
            string "  fun '(existT σ ";
            pp_id1;
            string ") '(existT τ ";
            pp_id2;
            string ") =>";
          ];
        indent @@ horizontal [ Coq.pp_match_pair (pp_id1, pp_id2) cases; Coq.eol ];
        string "Proof. all: transparent_abstract (intros H; depelim H). Defined."
      ]
    )
  end


let pp_reg_finite (register_names : PP.document list) : PP.document GC.t =
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
    Coq.pp_list (List.map ~f:enum_value_of_register_name register_names)
  in
  GC.generation_block [%here] (PP.string "REG_finite Instance") begin
    PP.(
      Coq.pp_sentence @@ separate hardline (
        [
          utf8string "Program Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :=";
          PP.indent begin
            Coq.pp_record_value [
              (string "enum", enum_values)
            ]
          end
        ]
      )
    )
  end


let pp_obligation_tactic () : PP.document GC.t =
  GC.generation_block [%here] (PP.string "Obligation Tactic") begin
    PP.vertical_strings [
        "Local Obligation Tactic :=";
        "  finite_from_eqdec."
      ]
  end


let pp_regdeclkit (register_definitions : (Sail.sail_definition * Ast.Definition.Register.t) list) : PP.document GC.t =
  let register_names =
    let extract_identifier (pair : Sail.sail_definition * Ast.Definition.Register.t) =
      Identifier.pp (snd pair).identifier
    in
    List.map ~f:extract_identifier register_definitions
  in
  let* section_contents =
    let* items = GC.sequence [
      pp_reg_inductive_type @@ List.map ~f:snd register_definitions;
      pp_no_confusion_for_reg ();
      pp_reg_definition ();
      pp_instance_reg_eq_dec register_names;
      pp_obligation_tactic ();
      pp_reg_finite register_names
    ]
    in
    GC.return @@ PP.vertical ~separator:PP.(twice hardline) items
  in
  GC.return @@ Coq.pp_section (Ast.Identifier.mk "RegDeclKit") section_contents


let extra_eqdec_identifiers () : Ast.Identifier.t list =
  [ regname_inductive_type_identifier ]


let extra_no_confusion_identifiers () : Ast.Identifier.t list =
  [ regname_inductive_type_identifier ]


let pp_register_finiteness (register_definitions : (Sail.sail_definition * Ast.Definition.Register.t) list) : PP.document GC.t =
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
  GC.return @@ Coq.pp_finite_instance ~identifier ~type_name ~values
