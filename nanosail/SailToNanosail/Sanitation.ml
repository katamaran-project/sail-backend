open Base
open Auxlib

module Big_int = Nat_big_num


module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module N = Ast


(* Renames variables so that the identifiers are valid in Coq *)
let coqify_identifiers (program : N.program) : N.program =
  let sanitize_type_definition (type_definition : N.type_definition) : N.type_definition =
    let sanitize_type_abbreviation (type_abbreviation_definition : N.type_abbreviation_definition) : N.type_abbreviation_definition =
      let identifier = type_abbreviation_definition.identifier
      and abbreviation = type_abbreviation_definition.abbreviation
      in
      match abbreviation with
      | N.TA_numeric_expression (type_quantifier, numeric_expression) ->
        begin
          let type_quantifier', numeric_expression' = Substitute.Sanitize.numeric_expression type_quantifier numeric_expression
          in
          {
            identifier = identifier;
            abbreviation = N.TA_numeric_expression (type_quantifier', numeric_expression')
          }
        end
      | N.TA_numeric_constraint (type_quantifier, numeric_constraint) ->
        begin
          let type_quantifier', numeric_constraint' = Substitute.Sanitize.numeric_constraint type_quantifier numeric_constraint
          in
          {
            identifier = identifier;
            abbreviation = N.TA_numeric_constraint (type_quantifier', numeric_constraint')
          }
        end
      | N.TA_alias (type_quantifier, nanotype) ->
        begin
          let type_quantifier', nanotype' = Substitute.Sanitize.nanotype type_quantifier nanotype
          in
          {
            identifier = identifier;
            abbreviation = N.TA_alias (type_quantifier', nanotype')
          }
        end
    in
    let sanitize_variant (variant_definition : N.variant_definition) : N.variant_definition =
      let identifier      = variant_definition.identifier
      and type_quantifier = variant_definition.type_quantifier
      and constructors    = variant_definition.constructors
      in
      let type_quantifier', subst = Substitute.process_type_quantifier Substitute.sanitize_identifier type_quantifier
      in
      let sanitize_constructor (constructor_identifier, constructor_nanotype) =
        (
          constructor_identifier,
          Substitute.Subst.nanotype subst constructor_nanotype
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
    let sanitize_enum (enum_definition : N.enum_definition) : N.enum_definition =
      (* no work to be done *)
      enum_definition
    in
    match (type_definition : N.type_definition) with
    | N.TD_abbreviation abbreviation -> N.TD_abbreviation (sanitize_type_abbreviation abbreviation)
    | N.TD_variant variant           -> N.TD_variant (sanitize_variant variant)
    | N.TD_enum enum                 -> N.TD_enum (sanitize_enum enum)
  in
  let sanitize_definition
      (sail_definition : N.sail_definition)
      (definition      : N.definition     ) : N.sail_definition * N.definition =
    (
      sail_definition,
      match definition with
      | N.TypeDefinition def                 -> N.TypeDefinition (sanitize_type_definition def)
      | N.TopLevelTypeConstraintDefinition _ -> definition
      | N.FunctionDefinition _               -> definition
      | N.RegisterDefinition _               -> definition
      | N.UntranslatedDefinition _           -> definition
      | N.IgnoredDefinition                  -> definition
      | N.ValueDefinition _                  -> definition
    )
  in
  {
    program_name = program.program_name;
    definitions  = List.map ~f:(uncurry sanitize_definition) program.definitions
  }
