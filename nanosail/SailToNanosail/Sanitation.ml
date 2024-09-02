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
  let sanitize_type_definition (type_definition : Ast.Definition.Type.t) : Ast.Definition.Type.t =
    let sanitize_type_abbreviation (type_abbreviation_definition : Ast.Definition.Type.Abbreviation.t) : Ast.Definition.Type.Abbreviation.t =
      let identifier = type_abbreviation_definition.identifier
      and abbreviation = type_abbreviation_definition.abbreviation
      in
      match abbreviation with
      | NumericExpression (type_quantifier, numeric_expression) ->
        begin
          let type_quantifier', numeric_expression' = Substitute.Sanitize.numeric_expression type_quantifier numeric_expression
          in
          {
            identifier = identifier;
            abbreviation = Ast.Definition.Type.Abbreviation.NumericExpression (type_quantifier', numeric_expression')
          }
        end
      | NumericConstraint (type_quantifier, numeric_constraint) ->
        begin
          let type_quantifier', numeric_constraint' = Substitute.Sanitize.numeric_constraint type_quantifier numeric_constraint
          in
          {
            identifier = identifier;
            abbreviation = Ast.Definition.Type.Abbreviation.NumericConstraint (type_quantifier', numeric_constraint')
          }
        end
      | Alias (type_quantifier, nanotype) ->
        begin
          let type_quantifier', nanotype' = Substitute.Sanitize.nanotype type_quantifier nanotype
          in
          {
            identifier = identifier;
            abbreviation = Ast.Definition.Type.Abbreviation.Alias (type_quantifier', nanotype')
          }
        end
    in
    let sanitize_variant (variant_definition : Ast.Definition.Type.Variant.t) : Ast.Definition.Type.Variant.t =
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
    let sanitize_enum (enum_definition : Ast.Definition.Type.Enum.t) : Ast.Definition.Type.Enum.t =
      (* no work to be done *)
      enum_definition
    in
    let sanitize_record (record_definition : Ast.Definition.Type.Record.t) : Ast.Definition.Type.Record.t =
      (* todo *)
      Stdio.printf "WARNING: record %s may need to be sanitized\n" (Ast.Identifier.string_of record_definition.identifier);
      record_definition
    in
    match type_definition with
    | Abbreviation abbreviation -> Ast.Definition.Type.Abbreviation (sanitize_type_abbreviation abbreviation)
    | Variant variant           -> Ast.Definition.Type.Variant         (sanitize_variant variant               )
    | Enum enum                 -> Ast.Definition.Type.Enum            (sanitize_enum enum                     )
    | Record record             -> Ast.Definition.Type.Record          (sanitize_record record                 )
  in
  let sanitize_definition
      (sail_definition : Sail.sail_definition)
      (definition      : Ast.Definition.t    ) : Sail.sail_definition * Ast.Definition.t =
    (
      sail_definition,
      match definition with
      | TypeDefinition def                 -> Ast.Definition.TypeDefinition (sanitize_type_definition def)
      | TopLevelTypeConstraintDefinition _ -> definition
      | FunctionDefinition _               -> definition
      | RegisterDefinition _               -> definition
      | UntranslatedDefinition _           -> definition
      | IgnoredDefinition                  -> definition
      | ValueDefinition _                  -> definition
    )
  in
  {
    definitions  = List.map ~f:(uncurry sanitize_definition) program.definitions
  }
