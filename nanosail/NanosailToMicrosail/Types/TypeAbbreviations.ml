open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let generate (type_abbreviation : Ast.Definition.Type.Abbreviation.t) : PP.document AC.t =
  let { identifier; abbreviation } : Ast.Definition.Type.Abbreviation.t = type_abbreviation
  in
  match abbreviation with
  | TA_numeric_expression (quantifier, numexpr) -> begin
      let  identifier  = Identifier.pp identifier
      in
      let* body        = Numeric.Expression.pp numexpr
      and* parameters  = PPSail.pp_type_quantifier quantifier
      in
      AC.return @@ Coq.definition ~identifier ~parameters body
    end

  | TA_numeric_constraint (quantifier, numconstraint) -> begin
      let  identifier  = Identifier.pp identifier
      in
      let* body        = Numeric.Constraint.pp numconstraint
      and* parameters  = PPSail.pp_type_quantifier quantifier
      in
      AC.return @@ Coq.definition ~identifier ~parameters body
    end

  | TA_alias (quantifier, typ) -> begin
      let identifier  = Identifier.pp identifier
      in
      let* body =
        match typ with
        | Nanosail__Ast__Recursive.Type.Int                -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.Bool               -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.String             -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.List _             -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.Product (_, _)     -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.Sum (_, _)         -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.Unit               -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.Enum id            -> AC.return @@ Identifier.pp id
        | Nanosail__Ast__Recursive.Type.Bitvector _        -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.Tuple _            -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.Variant _          -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.Record _           -> AC.not_yet_implemented [%here]
        | Nanosail__Ast__Recursive.Type.Application (_, _) -> AC.not_yet_implemented [%here]
      and* parameters = PPSail.pp_type_quantifier quantifier
      in
      AC.return @@ Coq.definition ~identifier ~parameters body;
    end
