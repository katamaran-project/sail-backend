open Base
open Auxlib

module Big_int = Nat_big_num


module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end


(* Renames variables so that the identifiers are valid in Coq *)
let coqify_identifiers (program : Ast.program) : Ast.program =
  let sanitize_type_definition (type_definition : Ast.type_definition) : Ast.type_definition =
    let sanitize_type_abbreviation (type_abbreviation_definition : Ast.type_abbreviation_definition) : Ast.type_abbreviation_definition =
      let identifier = type_abbreviation_definition.identifier
      and abbreviation = type_abbreviation_definition.abbreviation
      in
      match abbreviation with
      | Ast.TA_numeric_expression (type_quantifier, numeric_expression) ->
        begin
          let type_quantifier', numeric_expression' = Substitute.Sanitize.numeric_expression type_quantifier numeric_expression
          in
          {
            identifier = identifier;
            abbreviation = Ast.TA_numeric_expression (type_quantifier', numeric_expression')
          }
        end
      | Ast.TA_numeric_constraint (type_quantifier, numeric_constraint) ->
        begin
          let type_quantifier', numeric_constraint' = Substitute.Sanitize.numeric_constraint type_quantifier numeric_constraint
          in
          {
            identifier = identifier;
            abbreviation = Ast.TA_numeric_constraint (type_quantifier', numeric_constraint')
          }
        end
      | Ast.TA_alias (type_quantifier, nanotype) ->
        begin
          let type_quantifier', nanotype' = Substitute.Sanitize.nanotype type_quantifier nanotype
          in
          {
            identifier = identifier;
            abbreviation = Ast.TA_alias (type_quantifier', nanotype')
          }
        end
    in
    let sanitize_variant (variant_definition : Ast.variant_definition) : Ast.variant_definition =
      let identifier      = variant_definition.identifier
      and type_quantifier = variant_definition.type_quantifier
      and constructors    = variant_definition.constructors
      in
      let type_quantifier', subst = Substitute.process_type_quantifier Substitute.sanitize_identifier type_quantifier
      in
      let sanitize_constructor (constructor_identifier, constructor_fields) =
        (
          constructor_identifier,
          List.map ~f:(Substitute.Subst.nanotype subst) constructor_fields
        )
      in
      let constructors' =
        List.map ~f:sanitize_constructor constructors
      in
      {
        identifier      = identifier      ;
        type_quantifier = type_quantifier';
        constructors    = constructors'
      }
    in
    let sanitize_enum (enum_definition : Ast.enum_definition) : Ast.enum_definition =
      (* no work to be done *)
      enum_definition
    in
    let sanitize_record (record_definition : Ast.record_definition) : Ast.record_definition =
      (* todo *)
      Stdio.printf "WARNING: record %s may need to be sanitized\n" (Ast.Identifier.string_of record_definition.identifier);
      record_definition
    in
    match (type_definition : Ast.type_definition) with
    | Ast.TD_abbreviation abbreviation -> Ast.TD_abbreviation (sanitize_type_abbreviation abbreviation)
    | Ast.TD_variant variant           -> Ast.TD_variant      (sanitize_variant variant               )
    | Ast.TD_enum enum                 -> Ast.TD_enum         (sanitize_enum enum                     )
    | Ast.TD_record record             -> Ast.TD_record       (sanitize_record record                 )
  in
  let sanitize_definition
      (sail_definition : Sail.sail_definition)
      (definition      : Ast.definition        ) : Sail.sail_definition * Ast.definition =
    (
      sail_definition,
      match definition with
      | Ast.TypeDefinition def                 -> Ast.TypeDefinition (sanitize_type_definition def)
      | Ast.TopLevelTypeConstraintDefinition _ -> definition
      | Ast.FunctionDefinition _               -> definition
      | Ast.RegisterDefinition _               -> definition
      | Ast.UntranslatedDefinition _           -> definition
      | Ast.IgnoredDefinition                  -> definition
      | Ast.ValueDefinition _                  -> definition
    )
  in
  {
    program_name = program.program_name;
    definitions  = List.map ~f:(uncurry sanitize_definition) program.definitions
  }
