open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let generate (type_abbreviation : Ast.Definition.Type.Abbreviation.t) : PP.document AC.t =
  let { identifier; abbreviation } : Ast.Definition.Type.Abbreviation.t = type_abbreviation
  in
  match abbreviation with
  | TA_numeric_expression (quantifier, numexpr) -> begin
      let  identifier  = Identifier.pp_identifier identifier
      in
      let* body        = Numeric.Expression.pp numexpr
      and* parameters  = PPSail.pp_type_quantifier quantifier
      in
      AC.return @@ Coq.definition ~identifier ~parameters body
    end

  | TA_numeric_constraint (quantifier, numconstraint) -> begin
      let  identifier  = Identifier.pp_identifier identifier
      in
      let* body        = Numeric.Constraint.pp numconstraint
      and* parameters  = PPSail.pp_type_quantifier quantifier
      in
      AC.return @@ Coq.definition ~identifier ~parameters body
    end

  | TA_alias (quantifier, typ) -> begin
      let  identifier  = Identifier.pp_identifier identifier
      in
      let* body        = Nanotype.pp_nanotype typ
      and* parameters  = PPSail.pp_type_quantifier quantifier
      in
      AC.return @@ Coq.definition ~identifier ~parameters body;
    end

