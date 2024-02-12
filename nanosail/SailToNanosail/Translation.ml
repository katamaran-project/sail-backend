open Variant (* must be opened before Base due to name clash *)

open Base
open Basics

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast

module TC = TranslationContext
open Monads.Notations.Star(TC)
open Identifier
open Nanotype
open Expression
open Function
open TypeQuantifier
open TypeAbbreviation
open Enum
open Record


let translate_record
      (_definition_annotation : S.def_annot              )
      (_type_annotation       : N.type_annotation S.annot)
      (identifier             : S.id                     )
      (type_quantifier        : S.typquant               )
      (fields                 : (S.typ * S.id) list      ) : N.type_definition TC.t
  =
  let translate_field (field_type : S.typ) (field_identifier : S.id) =
    let* field_type'       = nanotype_of_sail_type field_type
    and* field_identifier' = translate_identifier [%here] field_identifier
    in
    TC.return @@ (field_identifier', field_type')
  in
  let* identifier      = translate_identifier [%here] identifier
  and* type_quantifier = translate_type_quantifier type_quantifier
  and* fields          = TC.map ~f:(Auxlib.uncurry translate_field) fields
  in
  TC.return @@ N.TD_record {
                   identifier;
                   type_quantifier;
                   fields
                 }


let translate_type_definition
      (definition_annotation     : S.def_annot                 )
      (annotated_type_definition : N.type_annotation S.type_def) : N.definition TC.t
  =
  let S.TD_aux (type_definition, type_annotation) = annotated_type_definition
  in
  let register translation =
    let* result = translation
    in
    let* () = TC.register_type result
    in
    TC.return @@ N.TypeDefinition result
  in
  match type_definition with
  | TD_abbrev (identifier, quantifier, arg)                      -> register @@ translate_type_abbreviation definition_annotation type_annotation identifier quantifier arg
  | TD_variant (identifier, type_quantifier, constructors, flag) -> register @@ translate_variant definition_annotation type_annotation identifier type_quantifier constructors flag
  | TD_enum (identifier, cases, _)                               -> register @@ translate_enum definition_annotation type_annotation identifier cases
  | TD_record (identifier, quantifier, fields, _)                -> register @@ translate_record definition_annotation type_annotation identifier quantifier fields
  | TD_bitfield (_, _, _)                                        -> TC.not_yet_implemented [%here] definition_annotation.loc

let translate_top_level_type_constraint
      (_definition_annotation : S.def_annot)
      (S.VS_aux (value_specification, _vspec_annotation)) : N.definition TC.t =
  let VS_val_spec (
          TypSchm_aux (
              TypSchm_ts (_quantifiers, Typ_aux (_typ, _type_location)),
              _type_scheme_location),
          identifier, _extern) = value_specification
  in
  let* identifier' = translate_identifier [%here] identifier
  in
  TC.return @@ N.TopLevelTypeConstraintDefinition { identifier = identifier' }

let translate_register
      (_definition_annotation        : S.def_annot                 )
      (annotated_register_definition : N.type_annotation S.dec_spec) : N.definition TC.t
  =
  let (S.DEC_aux (DEC_reg (sail_type, identifier, expression), (_spec_location, _spec_annotation))) = annotated_register_definition
  in
  match expression with
  | None -> begin
      let* identifier' = translate_identifier [%here] identifier
      and* nanotype    = nanotype_of_sail_type sail_type
      in
      TC.return @@ N.RegisterDefinition {
          identifier = identifier';
          typ        = nanotype   ;
        }
    end
  | Some (E_aux (_expr, (location, _annotation))) -> TC.not_yet_implemented [%here] location


let translate_mapping_definition
      (_definition_annotation : S.def_annot)
      (S.MD_aux (_definition, (location, _mapping_annotation))) =
  TC.not_yet_implemented [%here] location

let translate_impl_definition
      (_definition_annotation : S.def_annot)
      (S.FCL_aux (_definition, (annot, _))) =
  TC.not_yet_implemented [%here] annot.loc

let translate_value_definition
      (_definition_annotation : S.def_annot)
      (let_definition : N.type_annotation S.letbind)
  =
  let S.LB_aux (S.LB_val (S.P_aux (pattern, (pattern_location, _)), expression), (_location, _type_annotation)) = let_definition
  in
  match pattern with
  | S.P_id identifier -> begin
      match identifier with
      | S.Id_aux (S.Id identifier, _identifier_location)  -> begin
          let* value = translate_expression expression
          in
          TC.return @@ N.ValueDefinition { identifier; value }
        end
      | S.Id_aux (S.Operator _, _) -> TC.not_yet_implemented [%here] pattern_location
    end
  | S.P_lit _                      -> TC.not_yet_implemented [%here] pattern_location
  | S.P_wild                       -> TC.not_yet_implemented [%here] pattern_location
  | S.P_or (_, _)                  -> TC.not_yet_implemented [%here] pattern_location
  | S.P_not _                      -> TC.not_yet_implemented [%here] pattern_location
  | S.P_as (_, _)                  -> TC.not_yet_implemented [%here] pattern_location
  | S.P_typ (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
  | S.P_var (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
  | S.P_app (_, _)                 -> TC.not_yet_implemented [%here] pattern_location
  | S.P_vector _                   -> TC.not_yet_implemented [%here] pattern_location
  | S.P_vector_concat _            -> TC.not_yet_implemented [%here] pattern_location
  | S.P_vector_subrange (_, _, _)  -> TC.not_yet_implemented [%here] pattern_location
  | S.P_tuple _                    -> TC.not_yet_implemented [%here] pattern_location
  | S.P_list _                     -> TC.not_yet_implemented [%here] pattern_location
  | S.P_cons (_, _)                -> TC.not_yet_implemented [%here] pattern_location
  | S.P_string_append _            -> TC.not_yet_implemented [%here] pattern_location
  | S.P_struct (_, _)              -> TC.not_yet_implemented [%here] pattern_location


let translate_top_level_outcome_definition
      (_definition_annotation : S.def_annot)
      (S.OV_aux (_outcome, location))
      (_definitions : ('a S.def) list) =
  TC.not_yet_implemented [%here] location

let translate_definition (S.DEF_aux (def, annotation) as sail_definition) : (N.sail_definition * N.definition) TC.t =
  if
    Configuration.ignore_definition sail_definition
  then
    TC.return (sail_definition, N.IgnoredDefinition)
  else begin
    let translation =
      let* result =
        match def with
        | DEF_type type_definition                 -> translate_type_definition annotation type_definition
        | DEF_mapdef definition                    -> translate_mapping_definition annotation definition
        | DEF_impl impl_definition                 -> translate_impl_definition annotation impl_definition
        | DEF_let let_definition                   -> translate_value_definition annotation let_definition
        | DEF_val value_specification              -> translate_top_level_type_constraint annotation value_specification
        | DEF_outcome (outcome_spec, definitions)  -> translate_top_level_outcome_definition annotation outcome_spec definitions
        | DEF_register specification               -> translate_register annotation specification
        | DEF_fundef function_definition           -> translate_function_definition annotation function_definition
        | DEF_instantiation (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_fixity (_, _, _)                     -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_overload (_, _)                      -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_default _                            -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_scattered _                          -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_measure (_, _, _)                    -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_loop_measures (_, _)                 -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_internal_mutrec _                    -> TC.not_yet_implemented [%here] annotation.loc
        | DEF_pragma (pragma, _argument, location) -> TC.not_yet_implemented ~message:("pragma " ^ pragma) [%here] location
      in
      TC.return (sail_definition, result)
    in
    TC.recover translation begin fun error ->
      match error with
      | TC.NotYetImplemented (ocaml_location, sail_location, message) -> begin
          let untranslated_definition = N.UntranslatedDefinition {
              filename = ocaml_location.pos_fname;
              line_number = ocaml_location.pos_lnum;
              sail_location = sail_location;
              message = message
            }
          in
          TC.return (sail_definition, untranslated_definition)
        end
      | TC.AssertionFailure (ocaml_location, message) -> begin
          let location_string =
            Printf.sprintf "%s line %d" ocaml_location.pos_fname ocaml_location.pos_lnum
          and pretty_printed_sail_code =
            string_of_sail_definition sail_definition
          in
          failwith @@ Printf.sprintf "Assertion error at %s\nMessage: %s\nSail code:\n%s" location_string message pretty_printed_sail_code
        end
    end
  end

let translate (ast : Libsail.Type_check.tannot Libsail.Ast_defs.ast) name : N.program =
  let translate =
    let* () = Prelude.register_types ()
    in
    TC.map ~f:translate_definition ast.defs
  in
  let (result, _context) = TC.run translate
  in
  match result with
  | TC.Success definitions -> { program_name = name; definitions  = definitions }
  | TC.Failure _           -> failwith "Bug: failures should have been recovered from earlier"
