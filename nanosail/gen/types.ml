open PPrint
open Ast
open Monad


let pp_type_definition (original : sail_definition) (type_definition : type_definition) : document =
  let document =
    match type_definition with
    | TD_abbreviation (identifier, type_abbreviation) ->
       begin
         match type_abbreviation with
         | TA_numeric_expression (quantifier, numexpr) ->
            let  identifier  = Sail.pp_identifier identifier
            and  result_type = None in
            let* body        = Sail.pp_numeric_expression numexpr
            and* parameters  = Sail.pp_type_quantifier quantifier
            in
            generate @@ Coq.definition ~identifier ~parameters ~result_type ~body
         
         | TA_numeric_constraint (quantifier, numconstraint) ->
            let  identifier  = Sail.pp_identifier identifier
            and  result_type = None in
            let* body        = Sail.pp_numeric_constraint numconstraint
            and* parameters  = Sail.pp_type_quantifier quantifier
            in
            generate @@ Coq.definition ~identifier ~parameters ~result_type ~body
         
         | TA_alias (quantifier, typ) ->
            let  identifier  = Sail.pp_identifier identifier
            and  result_type = None in
            let* body        = Sail.pp_nanotype typ
            and* parameters  = Sail.pp_type_quantifier quantifier
            in
            generate @@ Coq.definition ~identifier ~parameters ~result_type ~body;
       end
  in
  Coq.annotate_with_original_definition original (Coq.annotate document)
