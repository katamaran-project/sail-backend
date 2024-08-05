open Base
open Monads.Notations.Star(GenerationContext)

module GC = GenerationContext


let generate (type_abbreviation : Ast.Definition.Type.Abbreviation.t) : PP.document GC.t =
  let { identifier; abbreviation } : Ast.Definition.Type.Abbreviation.t = type_abbreviation
  in
  match abbreviation with
  | NumericExpression (quantifier, numexpr) -> begin
      let  identifier  = Identifier.pp identifier
      in
      let* body        = Numeric.Expression.pp numexpr
      and* parameters  = PPSail.pp_type_quantifier quantifier
      in
      GC.return @@ Coq.definition ~identifier ~parameters body
    end

  | NumericConstraint (quantifier, numconstraint) -> begin
      let  identifier  = Identifier.pp identifier
      in
      let* body        = Numeric.Constraint.pp numconstraint
      and* parameters  = PPSail.pp_type_quantifier quantifier
      in
      GC.return @@ Coq.definition ~identifier ~parameters body
    end

  | Alias (quantifier, typ) -> begin
      let* coq_alias =
        let  identifier  = Identifier.pp identifier
        in
        let* body        = Nanotype.coq_type_of_nanotype typ
        and* parameters  = PPSail.pp_type_quantifier quantifier
        in
        GC.return @@ Coq.definition ~identifier ~parameters body;
      in
      GC.return coq_alias
    end
