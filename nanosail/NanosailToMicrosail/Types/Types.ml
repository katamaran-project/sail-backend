open Base
open PPrint
open Ast
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module PP = PPrint

module Variants = Variants

module TypeAbbreviations = struct
  let generate (type_abbreviation : type_abbreviation_definition) : AC.annotation AC.t =
    let { identifier; abbreviation } = type_abbreviation
    in
    match abbreviation with
    | TA_numeric_expression (quantifier, numexpr) -> begin
        let  identifier  = pp_identifier identifier
        and  result_type = None in
        let* body        = Numeric.pp_numeric_expression numexpr
        and* parameters  = Sail.pp_type_quantifier quantifier
        in
        AC.return @@ Coq.definition ~identifier ~parameters ~result_type ~body
      end

    | TA_numeric_constraint (quantifier, numconstraint) -> begin
        let  identifier  = pp_identifier identifier
        and  result_type = None in
        let* body        = Numeric.pp_numeric_constraint numconstraint
        and* parameters  = Sail.pp_type_quantifier quantifier
        in
        AC.return @@ Coq.definition ~identifier ~parameters ~result_type ~body
      end

    | TA_alias (quantifier, typ) -> begin
        let  identifier  = pp_identifier identifier
        and  result_type = None in
        let* body        = Nanotype.pp_nanotype typ
        and* parameters  = Sail.pp_type_quantifier quantifier
        in
        AC.return @@ Coq.definition ~identifier ~parameters ~result_type ~body;
      end
end


module Enums = struct
  let generate (enum_definition : enum_definition) : AC.annotation AC.t =
    let identifier = pp_identifier enum_definition.identifier
    and typ = pp_identifier "Set"
    in
    Coq.mbuild_inductive_type identifier typ (fun add_constructor ->
        AC.iter ~f:add_constructor @@ List.map ~f:string enum_definition.cases
      )

  let generate_enum_of_enums (enum_definitions : (sail_definition * enum_definition) list) =
    let enum_definitions =
      List.map ~f:snd enum_definitions
    in
    let identifier = string "Enums"
    and typ = string "Set"
    and constructor_of_enum (enum_definition : enum_definition) =
      string @@ "E" ^ String.lowercase enum_definition.identifier
    in
    let inductive_type =
    Coq.mbuild_inductive_type
      identifier
      typ
      (fun add_constructor ->
         AC.iter
           ~f:(fun enum_identifier ->
               add_constructor (constructor_of_enum enum_identifier)
             )
           enum_definitions
      )
    in
    Coq.annotate inductive_type

  let generate_no_confusions (enum_definitions : (sail_definition * enum_definition) list) =
    let enum_definitions = List.map ~f:snd enum_definitions
    in
    let contents =
      let set_transparent_obligations =
        string "Local Set Transparent Obligations."
      in
      let derivations =
        let generate_derivation (enum_definition : enum_definition) =
          string @@ Printf.sprintf "Derive NoConfusion for %s." enum_definition.identifier
        in
        let lines =
          List.map ~f:generate_derivation enum_definitions
        in
        separate hardline lines
      in
      set_transparent_obligations ^^ twice hardline ^^ derivations
    in
    Coq.section "TransparentObligations" contents

  let generate_eqdecs (enum_definitions : (sail_definition * enum_definition) list) =
    let enum_definitions = List.map ~f:snd enum_definitions (* todo cleanup *)
    in
    let generate_eqdec (enum_definition : enum_definition) =
      string @@ Printf.sprintf "Derive EqDec for %s." enum_definition.identifier
    in
    let lines =
      List.map ~f:generate_eqdec enum_definitions
    in
    separate hardline lines
end


module Records = struct
  let generate (record_definition : record_definition) : document AC.t =
    let generate_field field_identifier field_type =
      let* field_type' = Nanotype.coq_type_of_nanotype field_type
      in
      AC.return (string field_identifier, field_type')
    in
    let* identifier = AC.return @@ PP.string @@ record_definition.identifier
    and* type_name = AC.return @@ PP.string "Set"
    and* constructor = AC.return @@ PP.string @@ "Mk" ^ record_definition.identifier (* todo: allow custom name *)
    and* fields = AC.map ~f:(Auxlib.uncurry generate_field) record_definition.fields
    in
    AC.return @@ Coq.record ~identifier ~type_name ~constructor ~fields
end


let pp_type_definition
      (original        : sail_definition)
      (type_definition : type_definition) : document
  =
  let document =
    match type_definition with
    | TD_abbreviation abbrev -> TypeAbbreviations.generate abbrev
    | TD_enum enum           -> Enums.generate enum
    | TD_variant variant     -> Variants.generate variant
    | TD_record record       -> Records.generate record
  in
  Coq.annotate_with_original_definition original (Coq.annotate document)
