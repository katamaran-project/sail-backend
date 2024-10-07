open Base

module GC = GenerationContext


module rec Expression : sig
  val pp : Ast.Numeric.Expression.t -> PP.document GC.t
end = struct
  let pp (numeric_expression : Ast.Numeric.Expression.t) =
    let rec pp level (numexp : Ast.Numeric.Expression.t) =
      let parens_if lvl doc =
        if level <= lvl
        then doc
        else PP.(surround parens) doc
      in
      match numexp with
      | Constant z   -> PP.string (Z.to_string z)
      | Add (x, y)   -> parens_if 0 @@ PP.pp_binary_operation Coq.Operator.addition       [pp 0 x; pp 0 y]
      | Minus (x, y) -> parens_if 0 @@ PP.pp_binary_operation Coq.Operator.subtraction    [pp 0 x; pp 0 y]
      | Times (x, y) -> parens_if 1 @@ PP.pp_binary_operation Coq.Operator.multiplication [pp 1 x; pp 1 y]
      | Neg x        -> parens_if 2 @@ PP.(horizontal [ minus; pp 3 x])
      | Id id        -> Identifier.pp id
      | Var id       -> Identifier.pp id
    in
    GC.return @@ pp 0 numeric_expression
end

and Constraint : sig
  val pp : Ast.Numeric.Constraint.t -> PP.document GC.t
end = struct
 let pp (numeric_constraint : Ast.Numeric.Constraint.t) =
   match numeric_constraint with
   | True                -> GC.return @@ PP.string "true"
   | False               -> GC.return @@ PP.string "false"
   | Equal (_x, _y)      -> GC.not_yet_implemented [%here]
   | BoundedGE (_x, _y)  -> GC.not_yet_implemented [%here]
   | BoundedGT (_x, _y)  -> GC.not_yet_implemented [%here]
   | BoundedLE (_x, _y)  -> GC.not_yet_implemented [%here]
   | BoundedLT (_x, _y)  -> GC.not_yet_implemented [%here]
   | NotEqual  (_x, _y)  -> GC.not_yet_implemented [%here]
   | Set (_x, _y)        -> GC.not_yet_implemented [%here]
   | Or (_x, _y)         -> GC.not_yet_implemented [%here]
   | And (_x, _y)        -> GC.not_yet_implemented [%here]
   | App (_x, _y)        -> GC.not_yet_implemented [%here]
   | Var _               -> GC.not_yet_implemented [%here]
end
