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
          (*
             A right operand at the same level is parenthesised: 8 * (n / 8) is not (8 * n) / 8.
             Only a product to the right of a product needs none (multiplication is associative).
          *)
          | Mul    -> begin
              let right_level =
                match y with
                | BinaryOperation (Mul, _, _)                   -> 1
                | BinaryOperation ((Add | Sub | Div | Mod), _, _) -> 2
                | Constant _ | Neg _ | PowerOf2 _ | Id _ | Var _ -> 2
              in
              parens_if 1 @@ PP.binary_operation Coq.Operator.multiplication [pp 1 x; pp right_level y]
            end
          | Div    -> parens_if 1 @@ PP.binary_operation Coq.Operator.division       [pp 1 x; pp 2 y]
          | Mod    -> parens_if 1 @@ PP.binary_operation Coq.Operator.modulo         [pp 1 x; pp 2 y]
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
    (*
       Inside a function definition, a type-level variable (kid__n) that the function's signature does not
       bind cannot be printed: Rocq would not know it. This happens when a width depends on the value of a
       parameter (val f : forall 'n. (int('n), ...) with bits('n) in the body), which muSail types cannot
       express (P13), and for Sail's existential variables. Such a width is a hole.
    *)
    let* bound = GC.get_bound_type_level_identifiers
    in
    let unbound_type_level_identifiers =
      match bound with
      | None       -> []
      | Some bound -> begin
          List.filter (TypeLevelParameters.numeric_identifiers numeric_expression) ~f:(fun identifier ->
              String.is_prefix (Ast.Identifier.to_string identifier) ~prefix:"kid__"
              && not (Ast.Identifier.Set.mem bound identifier)
            )
        end
    in
    (*
       Inside a function definition, a division or modulus of a size variable, as in Sail's bits(8 * ('p / 8))
       for a 'p that its constraints make a multiple of 8, is a hole too: Rocq would need those constraints to
       identify such a width with 'p, and they are not translated (P13).
    *)
    let rec divides_a_variable (numeric_expression : Ast.Numeric.Expression.t) : bool =
      match numeric_expression with
      | Constant _                                        -> false
      | Id _                                              -> false
      | Var _                                             -> false
      | Neg operand                                       -> divides_a_variable operand
      | PowerOf2 operand                                  -> divides_a_variable operand
      | BinaryOperation ((Div | Mod), left, right)        -> not (List.is_empty (TypeLevelParameters.numeric_identifiers left @ TypeLevelParameters.numeric_identifiers right))
      | BinaryOperation ((Add | Sub | Mul), left, right)  -> divides_a_variable left || divides_a_variable right
    in
    match unbound_type_level_identifiers with
    | [] when Option.is_some bound && divides_a_variable numeric_expression -> begin
        GC.not_yet_implemented ~message:"the width divides a size variable, which needs Sail's constraints (P13)" [%here]
      end
    | [] -> GC.return @@ PP.annotate [%here] @@ pp initial_level numeric_expression
    | unbound :: _ -> begin
        let message =
          Printf.sprintf "the width depends on %s, which the function's signature does not bind (a value-dependent or existential size)" (Ast.Identifier.to_string unbound)
        in
        GC.not_yet_implemented ~message [%here]
      end

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
