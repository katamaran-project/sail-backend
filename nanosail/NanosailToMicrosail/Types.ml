open Base
open PPrint
open Ast
open Basics
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


module Variants = struct
  let pp_definition (variant_definition : variant_definition) : AC.annotation AC.t =
    let { identifier; type_quantifier; constructors } = variant_definition
    in
    let inductive_type =
      let identifier' =
        pp_identifier identifier
      in
      let pp_constructor_type (nanotype : nanotype) =
        let* ts = AC.map ~f:Nanotype.coq_type_of_nanotype @@ Ast.tuple_to_list nanotype
        in
        let ts = ts @ [ identifier' ]
        in
        AC.return @@ separate (string " -> ") ts
      in
      let* type_quantifier' =
        AC.map type_quantifier ~f:(fun (id, kind) ->
            let* kind' = Sail.pp_kind kind
            in
            AC.return (pp_identifier id, kind')
          )
      in
      Coq.mbuild_inductive_type
        identifier'
        ~parameters: type_quantifier'
        (string "Set")
        begin
          fun add_constructor ->
            AC.iter constructors ~f:(fun (constructor, typ) ->
                let* typ' = pp_constructor_type typ
                in
                add_constructor ~typ:typ' (string constructor))
        end
    in
    inductive_type
end


module TypeAbbreviations = struct
  let pp_definition (type_abbreviation : type_abbreviation_definition) : AC.annotation AC.t =
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
  let pp_definition (enum_definition : enum_definition) : AC.annotation AC.t =
    let identifier = pp_identifier enum_definition.identifier
    and typ = pp_identifier "Set"
    in
    Coq.mbuild_inductive_type identifier typ (fun add_constructor ->
        AC.iter ~f:add_constructor @@ List.map ~f:string enum_definition.cases
      )

  let generate_constructors_inductive_type (_sail_definition : sail_definition) (enum_definition : enum_definition) =
    let identifier = pp_identifier @@ enum_definition.identifier ^ "Constructor"
    and typ = pp_identifier "Set"
    in
    Coq.build_inductive_type identifier typ (fun add_constructor ->
        List.iter
          ~f:(fun (case : string) ->
              add_constructor @@ string @@ "K" ^ case)
          enum_definition.cases
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
    Coq.build_inductive_type
      identifier
      typ
      (fun add_constructor ->
         List.iter
           ~f:(fun enum_identifier ->
               add_constructor (constructor_of_enum enum_identifier)
             )
           enum_definitions
      )

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
    let enum_definitions = List.map ~f:snd enum_definitions
    in
    let generate_eqdec (enum_definition : enum_definition) =
      string @@ Printf.sprintf "Derive EqDec for %s." enum_definition.identifier
    in
    let lines =
      List.map ~f:generate_eqdec enum_definitions
    in
    separate hardline lines
end


let pp_type_definition (original : sail_definition) (type_definition : type_definition) : document =
  let document =
    match type_definition with
    | TD_abbreviation abbrev -> TypeAbbreviations.pp_definition abbrev
    | TD_enum enum           -> Enums.pp_definition enum
    | TD_variant variant     -> Variants.pp_definition variant
  in
  Coq.annotate_with_original_definition original (Coq.annotate document)
