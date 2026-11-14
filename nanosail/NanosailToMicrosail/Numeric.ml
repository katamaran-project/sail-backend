open ExtBase
open Monads.Notations.Star(GenerationContext)


module GC = GenerationContext


module rec Expression : sig
  val pp          : Ast.Numeric.Expression.t -> PP.t GC.t
  val pp_argument : Ast.Numeric.Expression.t -> PP.t GC.t
end = struct
  (*
     Precedence levels: 0 for + and -, 1 for *, / and mod, 2 for negation, 3 for 2^.
     An expression printed at level n is parenthesised if its operator has a lower level.
  *)
  let top_level      : int = 0
  let argument_level : int = 4

  let pp_at_level
      (initial_level      : int                      )
      (numeric_expression : Ast.Numeric.Expression.t ) : PP.t GC.t
    =
    let rec pp level (numexp : Ast.Numeric.Expression.t) : PP.t =
      let parens_if lvl doc =
        if level <= lvl
        then doc
        else PP.(surround parens) doc
      in
      match numexp with
      | Constant z -> begin
          if Z.lt z Z.zero
          then parens_if 2 @@ PP.string (Z.to_string z)
          else PP.string (Z.to_string z)
        end
      | BinaryOperation (op, x, y) -> begin
          match op with
          | Add    -> parens_if 0 @@ PP.binary_operation Coq.Operator.addition       [pp 0 x; pp 0 y]
          | Sub    -> parens_if 0 @@ PP.binary_operation Coq.Operator.subtraction    [pp 0 x; pp 1 y]
          | Mul    -> parens_if 1 @@ PP.binary_operation Coq.Operator.multiplication [pp 1 x; pp 1 y]
          | Div    -> parens_if 1 @@ PP.binary_operation Coq.Operator.division       [pp 1 x; pp 1 y]
          | Mod    -> parens_if 1 @@ PP.binary_operation Coq.Operator.modulo         [pp 1 x; pp 1 y]
        end
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
            GC.lookup_definition_opt (Ast.Definition.Select.(without_sail_definition @@ type_definition @@ of_abbreviation ~named:identifier @@ of_numeric_expression))
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
    GC.return @@ PP.annotate [%here] @@ pp initial_level numeric_expression

  let pp (numeric_expression : Ast.Numeric.Expression.t) : PP.t GC.t =
    pp_at_level top_level numeric_expression

  (*
     Prints the expression as the argument of a Rocq application,
     e.g., the width in bv (8 * 8): parenthesised unless it is atomic.
  *)
  let pp_argument (numeric_expression : Ast.Numeric.Expression.t) : PP.t GC.t =
    pp_at_level argument_level numeric_expression
end

and Constraint : sig
  val pp : Ast.Numeric.Constraint.t -> PP.t GC.t
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
