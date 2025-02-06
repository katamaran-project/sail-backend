open! ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(* Name for the inductive type listing all registers *)
let regname_inductive_type_identifier =
  Ast.Identifier.mk "RegName" (* todo make configurable *)


let regname_tag =
  Ast.Identifier.mk "regname" (* todo make configurable *)


(* todo improve name *)
let pp_reg_inductive_type (register_definitions : Ast.Definition.Register.t list) : PP.document GC.t =
  let identifier =
    PP.annotate [%here] @@ PP.string "Reg"
  and typ =
    PP.annotate [%here] @@ PP.string "Ty -> Set"
  in
  GC.generation_block [%here] "Reg Inductive Type" begin
    GC.block begin
      GC.pp_annotate [%here] begin
        GC.pp_inductive_type identifier typ (fun add_constructor ->
            let make_constructor (register_definition : Ast.Definition.Register.t) : unit GC.t =
              let identifier =
                PP.annotate [%here] @@ Identifier.pp register_definition.identifier
              in
              let* register_type =
                GC.pp_annotate [%here] @@ Nanotype.pp_nanotype register_definition.typ
              in
              let typ =
                PP.annotate [%here] begin
                  Coq.pp_application
                    (PP.string "Reg")
                    [ PP.(surround parens) register_type ]
                end
              in
              add_constructor ~typ:typ identifier
            in
            GC.iter ~f:make_constructor register_definitions
          )
      end
    end
  end


let pp_no_confusion_for_reg () : PP.document GC.t =
  GC.generation_block [%here] "No Confusion for Reg" begin
    GC.return begin
      Coq.pp_section (Ast.Identifier.mk "TransparentObligations") (
        PP.(vertical [
            string "Local Set Transparent Obligations.";
            string "Derive Signature NoConfusion NoConfusionHom EqDec for Reg."
          ])
      )
    end
  end


let pp_reg_definition () : PP.document GC.t =
  GC.block begin
      GC.return @@ PP.annotate [%here] @@ PP.string "Definition 𝑹𝑬𝑮 : Ty -> Set := Reg."
    end


let translate_regname (register_identifier : Ast.Identifier.t) : Ast.Identifier.t =
  let prefix = "RegName_"
  in
  Ast.Identifier.mk @@ Printf.sprintf "%s%s" prefix (Ast.Identifier.to_string register_identifier)

(*
  Defines RegName inductive type enumerating all registers

  (*
    Registers' initial values

    R1
      <initial value R1>
    R2
      <initial value R2>
    R3
      <initial value R3>
    R4
      <initial value R4>
 *)
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
  let* inductive_type =
    let type_name =
      PP.annotate [%here] begin
          Identifier.pp regname_inductive_type_identifier
        end
    and typ =
      PP.annotate [%here] begin
          PP.string "Set"
        end
    in
    GC.block begin
      GC.pp_inductive_type type_name typ (fun add_constructor ->
          GC.iter register_names ~f:(fun name -> add_constructor @@ Identifier.pp @@ translate_regname name)
        )
      end

  and* initial_values =
    let format_initial_value (initial_value : Ast.Definition.Register.initial_value) : PP.document GC.t =
      match initial_value with
      | NoneSpecified -> GC.return @@ PP.string "<no initial value specified>"
      | Specified value -> ValueDefinitions.pp_value value
      | RawSpecified raw_string -> GC.return @@ PP.horizontal [ PP.string "<raw> "; PP.string raw_string ]
    in
    let format_register_definition (register_definition : Ast.Definition.Register.t) : (PP.document * PP.document) GC.t =
      let register_id =
        Identifier.pp register_definition.identifier
      in
      let* initial_value =
        format_initial_value register_definition.initial_value
      in
      GC.return @@ (register_id, initial_value)
    in
    let* description_pairs =
      GC.map ~f:(Fn.compose format_register_definition snd) register_definitions
    in
    GC.return @@ PP.description_list description_pairs

  in
  GC.generation_block [%here] "Regname Inductive Type" begin
    GC.block begin
      GC.return begin
        PP.annotate [%here] begin
          PP.vertical [
            Coq.pp_multiline_comment @@ PP.vertical [
              PP.string "Registers' initial values";
              PP.string "";
              initial_values;
            ];
            inductive_type
          ]
        end
      end
    end
  end


let pp_instance_reg_eq_dec (register_names : PP.document list) : PP.document GC.t =
  let cases =
    let cs =
      List.map ~f:(fun register_name ->
          let rn_str = register_name in
          (
            (
              PP.annotate [%here] @@ rn_str,
              PP.annotate [%here] @@ rn_str
            ),
            PP.annotate [%here] @@ PP.string "left eq_refl"
          )
        )
        register_names
    and wildcard_case =
      PP.(
        (
          PP.annotate [%here] @@ string "_",
          PP.annotate [%here] @@ string "_"
        ),
        PP.annotate [%here] @@ string "right _"
      )
    in
    List.build_list (fun { add; addall; _ } ->
        addall cs;
        if List.length cs > 1
        then add wildcard_case;
      )
  in
  let id1 = Ast.Identifier.mk_generated "x"
  and id2 = Ast.Identifier.mk_generated "y"
  in
  let pp_id1 = PP.annotate [%here] @@ Identifier.pp id1
  and pp_id2 = PP.annotate [%here] @@ Identifier.pp id2
  in
  GC.block begin
    GC.generation_block [%here] "REG_eq_dec Instance" begin
      GC.return begin
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
    end
  end


let pp_reg_finite (register_names : PP.document list) : PP.document GC.t =
  let enum_values =
    let enum_value_of_register_name register_name =
      PP.annotate [%here] @@ Coq.pp_application
        (PP.string "existT")
        [
          PP.underscore;
          register_name
        ]
    in
    PP.annotate [%here] @@ Coq.pp_list (List.map ~f:enum_value_of_register_name register_names)
  in
  GC.block begin
    GC.generation_block [%here] "REG_finite Instance" begin
      GC.return begin
        PP.(
          Coq.pp_sentence @@ vertical (
            [
              string "Program Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :=";
              PP.indent begin
                Coq.pp_record_value [
                  (string "enum", enum_values)
                ]
              end
            ]
          )
        )
      end
    end
  end


let pp_obligation_tactic () : PP.document GC.t =
  GC.generation_block [%here] "Obligation Tactic" begin
    GC.return begin
      PP.vertical @@ List.map ~f:PP.string [
        "Local Obligation Tactic :=";
        "  finite_from_eqdec."
      ]
    end
  end


let pp_regdeclkit (register_definitions : (Sail.sail_definition * Ast.Definition.Register.t) list) : PP.document GC.t =
  let register_names =
    let extract_identifier (pair : Sail.sail_definition * Ast.Definition.Register.t) : PP.document =
      Identifier.pp (snd pair).identifier
    in
    List.map ~f:extract_identifier register_definitions
  in
  let* section_contents =
    let* items = GC.sequence [
      pp_reg_inductive_type @@ Ast.Definition.Select.drop_sail_definitions register_definitions;
      pp_no_confusion_for_reg ();
      pp_reg_definition ();
      pp_instance_reg_eq_dec register_names;
      pp_obligation_tactic ();
      pp_reg_finite register_names
    ]
    in
    GC.return @@ PP.annotate [%here] @@ PP.paragraphs items
  in
  GC.block begin
    GC.return begin
      PP.annotate [%here] begin
        Coq.pp_section (Ast.Identifier.mk "RegDeclKit") section_contents
      end
    end
  end


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
  let identifier = PP.annotate [%here] @@ Identifier.pp @@ Ast.Identifier.mk "RegName"
  and type_name  = PP.annotate [%here] @@ Identifier.pp @@ Ast.Identifier.mk "RegName"
  and values     = List.map ~f:(fun id -> PP.annotate [%here] @@ Identifier.pp id) translated_register_identifiers
  in
  GC.block begin
      GC.return @@ PP.annotate [%here] @@ Coq.pp_finite_instance ~identifier ~type_name ~values
    end
