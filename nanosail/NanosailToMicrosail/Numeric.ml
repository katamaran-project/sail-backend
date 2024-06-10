open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext


let pp_numeric_expression (numeric_expression : Ast.NumericExpression.t) =
  let rec pp level (numexp : Ast.NumericExpression.t) =
    let parens_if lvl doc =
      if level <= lvl
      then doc
      else PP.parens doc
    in
    match numexp with
    | Constant z   -> PP.string (Z.to_string z)
    | Add (x, y)   -> parens_if 0 @@ PP.(concat [ pp 0 x; space; plus; space; pp 0 y ])
    | Minus (x, y) -> parens_if 0 @@ PP.(concat [ pp 0 x; space; minus; space; pp 0 y ])
    | Times (x, y) -> parens_if 1 @@ PP.(concat [ pp 1 x; space; star; space; pp 1 y ])
    | Neg x        -> parens_if 2 @@ PP.(concat [ minus; pp 3 x ])
    | Id id        -> Identifier.pp_identifier id
    | Var id       -> Identifier.pp_identifier id
  in
  AC.return @@ pp 0 numeric_expression

and pp_numeric_constraint (numeric_constraint : Ast.NumericConstraint.t) =
  match numeric_constraint with
  | NC_true                -> AC.return @@ PP.string "true"
  | NC_false               -> AC.return @@ PP.string "false"
  | NC_equal (_x, _y)      -> AC.not_yet_implemented [%here]
  | NC_bounded_ge (_x, _y) -> AC.not_yet_implemented [%here]
  | NC_bounded_gt (_x, _y) -> AC.not_yet_implemented [%here]
  | NC_bounded_le (_x, _y) -> AC.not_yet_implemented [%here]
  | NC_bounded_lt (_x, _y) -> AC.not_yet_implemented [%here]
  | NC_not_equal (_x, _y)  -> AC.not_yet_implemented [%here]
  | NC_set (_x, _y)        -> AC.not_yet_implemented [%here]
  | NC_or (_x, _y)         -> AC.not_yet_implemented [%here]
  | NC_and (_x, _y)        -> AC.not_yet_implemented [%here]
  | NC_app (_x, _y)        -> AC.not_yet_implemented [%here]
  | NC_var _               -> AC.not_yet_implemented [%here]
