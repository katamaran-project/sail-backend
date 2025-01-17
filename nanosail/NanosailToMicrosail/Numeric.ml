open Base

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
      | Constant z -> PP.annotate [%here] @@ PP.string (Z.to_string z)
      | Add (x, y) -> PP.annotate [%here] @@ parens_if 0 @@ PP.pp_binary_operation Coq.Operator.addition       [pp 0 x; pp 0 y]
      | Sub (x, y) -> PP.annotate [%here] @@ parens_if 0 @@ PP.pp_binary_operation Coq.Operator.subtraction    [pp 0 x; pp 1 y]
      | Mul (x, y) -> PP.annotate [%here] @@ parens_if 1 @@ PP.pp_binary_operation Coq.Operator.multiplication [pp 1 x; pp 1 y]
      | Neg x      -> PP.annotate [%here] @@ parens_if 2 @@ PP.(horizontal [ minus; pp 3 x])
      | PowerOf2 x -> PP.annotate [%here] @@ parens_if 3 @@ PP.(horizontal [ PP.string "2^"; pp 4 x]) (* todo this notation probably needs fixing *)
      | Id id      -> PP.annotate [%here] @@ Identifier.pp id
      | Var id     -> PP.annotate [%here] @@ Identifier.pp id
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
