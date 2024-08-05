open Base
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module GC = GenerationContext


module rec Expression : sig
  val pp  : Ast.Numeric.Expression.t -> PP.document AC.t (* todo remove *)
  val pp' : Ast.Numeric.Expression.t -> PP.document GC.t (* todo rename *)
end = struct
  let pp (numeric_expression : Ast.Numeric.Expression.t) =
    let rec pp level (numexp : Ast.Numeric.Expression.t) =
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
      | Id id        -> Identifier.pp id
      | Var id       -> Identifier.pp id
    in
    AC.return @@ pp 0 numeric_expression

  let pp' (numeric_expression : Ast.Numeric.Expression.t) =
    let rec pp level (numexp : Ast.Numeric.Expression.t) =
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
      | Id id        -> Identifier.pp id
      | Var id       -> Identifier.pp id
    in
    GC.return @@ pp 0 numeric_expression
end

and Constraint : sig
  val pp  : Ast.Numeric.Constraint.t -> PP.document AC.t (* todo remove *)
  val pp' : Ast.Numeric.Constraint.t -> PP.document GC.t (* todo rename *)
end = struct
 let pp (numeric_constraint : Ast.Numeric.Constraint.t) =
   match numeric_constraint with
   | True                -> AC.return @@ PP.string "true"
   | False               -> AC.return @@ PP.string "false"
   | Equal (_x, _y)      -> AC.not_yet_implemented [%here]
   | BoundedGE (_x, _y)  -> AC.not_yet_implemented [%here]
   | BoundedGT (_x, _y)  -> AC.not_yet_implemented [%here]
   | BoundedLE (_x, _y)  -> AC.not_yet_implemented [%here]
   | BoundedLT (_x, _y)  -> AC.not_yet_implemented [%here]
   | NotEqual  (_x, _y)  -> AC.not_yet_implemented [%here]
   | Set (_x, _y)        -> AC.not_yet_implemented [%here]
   | Or (_x, _y)         -> AC.not_yet_implemented [%here]
   | And (_x, _y)        -> AC.not_yet_implemented [%here]
   | App (_x, _y)        -> AC.not_yet_implemented [%here]
   | Var _               -> AC.not_yet_implemented [%here]

 let pp' (numeric_constraint : Ast.Numeric.Constraint.t) =
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
