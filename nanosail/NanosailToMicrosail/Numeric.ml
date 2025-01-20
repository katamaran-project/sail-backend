open Base
open Monads.Notations.Star(GenerationContext)


module GC = GenerationContext


module rec Expression : sig
  val pp : Ast.Numeric.Expression.t -> PP.document GC.t
end = struct
  let pp (numeric_expression : Ast.Numeric.Expression.t) : PP.document GC.t =
    let rec pp level (numexp : Ast.Numeric.Expression.t) : PP.document =
      let parens_if lvl doc =
        if level <= lvl
        then doc
        else PP.(surround parens) doc
      in
      match numexp with
      | Constant z -> PP.string (Z.to_string z)
      | Add (x, y) -> parens_if 0 @@ PP.pp_binary_operation Coq.Operator.addition       [pp 0 x; pp 0 y]
      | Sub (x, y) -> parens_if 0 @@ PP.pp_binary_operation Coq.Operator.subtraction    [pp 0 x; pp 1 y]
      | Mul (x, y) -> parens_if 1 @@ PP.pp_binary_operation Coq.Operator.multiplication [pp 1 x; pp 1 y]
      | Neg x      -> parens_if 2 @@ PP.(horizontal [ minus; pp 3 x])
      | PowerOf2 x -> parens_if 3 @@ PP.(horizontal [ PP.string "2^"; pp 4 x]) (* todo this notation probably needs fixing *)
      | Id id      -> Identifier.pp id
      | Var id     -> Identifier.pp id
    in
    let* numeric_expression =
      if
        Configuration.(get inline_definitions_in_notations)
      then
        (*
           Inline definitions if configuration requires it
        *)
        let lookup_substitution (identifier : Ast.Identifier.t) : (Ast.Identifier.t * Ast.Numeric.Expression.t) option GC.t =
          let* definition =
            GC.lookup_definition_opt (Ast.Definition.Select.(without_sail @@ type_definition @@ of_abbreviation ~named:identifier @@ of_numeric_expression))
          in
          let definition = Option.map ~f:snd definition
          in
          match definition with
          | None                               -> GC.return None
          | Some (type_quantifier, definition) -> begin
              match type_quantifier with
              | TypeQuantifier [] -> GC.return @@ Some (identifier, definition)
              | _                 -> GC.fail [%here] "no support for type quantifiers yet"
            end
        in
        let identifiers = Ast.Numeric.Expression.identifiers numeric_expression
        in
        let* substitutions : (Ast.Identifier.t * Ast.Definition.NumericExpression.t) list =
          let* pairs =
            GC.map ~f:lookup_substitution identifiers
          in
          GC.return @@ List.filter_opt pairs
        in
        GC.return begin
          List.fold
            substitutions
            ~init:numeric_expression
            ~f:(fun numeric_expression (identifier, substitution) -> Ast.Numeric.Expression.substitute_identifier identifier substitution numeric_expression)
        end
      else
        GC.return numeric_expression
    in
    GC.return @@ PP.annotate [%here] @@ pp 0 numeric_expression
end

and Constraint : sig
  val pp : Ast.Numeric.Constraint.t -> PP.document GC.t
end = struct
 let pp (numeric_constraint : Ast.Numeric.Constraint.t) =
   match numeric_constraint with
   | True                          -> GC.return @@ PP.annotate [%here] @@ PP.string "true"
   | False                         -> GC.return @@ PP.annotate [%here] @@ PP.string "false"
   | Equal (_x, _y)                -> GC.not_yet_implemented [%here]
   | NotEqual (_x, _y)             -> GC.not_yet_implemented [%here]
   | GreaterThanOrEqualTo (_x, _y) -> GC.not_yet_implemented [%here]
   | GreaterThan (_x, _y)          -> GC.not_yet_implemented [%here]
   | LessThanOrEqualTo (_x, _y)    -> GC.not_yet_implemented [%here]
   | LessThan (_x, _y)             -> GC.not_yet_implemented [%here]
   | Set (_x, _y)                  -> GC.not_yet_implemented [%here]
   | Or (_x, _y)                   -> GC.not_yet_implemented [%here]
   | And (_x, _y)                  -> GC.not_yet_implemented [%here]
   | App (_x, _y)                  -> GC.not_yet_implemented [%here]
   | Var _                         -> GC.not_yet_implemented [%here]
end
