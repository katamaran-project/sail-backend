open Base
open Ast
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module PP = PPrint


let generate (type_abbreviation : type_abbreviation_definition) : PP.document AC.t =
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

