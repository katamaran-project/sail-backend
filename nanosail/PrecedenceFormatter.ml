module type Output = sig
  type t

  val parenthesize : t -> t
end

module Make(O : Output) = struct
  module ExtendedIntegerNotations = ComparisonNotations.Make(ExtendedInteger)

  type output = O.t
  type ast = Ast of O.t * ExtendedInteger.t

  let output_of (ast : ast) : output =
    let Ast (x, _) = ast
    in
    x

  let level_of (ast : ast) : ExtendedInteger.t =
    let Ast (_, n) = ast
    in
    n

  let parenthesize (ast : ast) : ast =
    Ast (O.parenthesize @@ output_of ast, ExtendedInteger.PositiveInfinity)

  let define_left_associative_binary_operator
      (precedence : int                       )
      (formatter  : output -> output -> output) : ast -> ast -> ast
    =
    let open ExtendedIntegerNotations
    in
    let precedence = ExtendedInteger.Int precedence
    in
    let format (left : ast) (right : ast) : ast =
      let left' =
        output_of begin
          if level_of left << precedence
          then parenthesize left
          else left
        end
      and right' =
        output_of begin
          if level_of right <<= precedence
          then parenthesize right
          else right
        end
      in
      Ast (formatter left' right', precedence)
    in
    format

  let define_right_associative_binary_operator
      (precedence : int                       )
      (formatter  : output -> output -> output) : ast -> ast -> ast
    =
    let open ExtendedIntegerNotations
    in
    let precedence = ExtendedInteger.Int precedence
    in
    let format (left : ast) (right : ast) : ast =
      let left' =
        output_of begin
          if level_of left <<= precedence
          then parenthesize left
          else left
        end
      and right' =
        output_of begin
          if level_of right << precedence
          then parenthesize right
          else right
        end
      in
      Ast (formatter left' right', precedence)
    in
    format

  let define_atom (output : output) : ast =
    Ast (output, ExtendedInteger.PositiveInfinity)

  let define_unary_prefix_operator
      (precedence : int             )
      (formatter  : output -> output) : ast -> ast
    =
    let open ExtendedIntegerNotations
    in
    let precedence = ExtendedInteger.Int precedence
    in
    let format (operand : ast) : ast =
      let operand' =
        output_of begin
          if level_of operand << precedence
          then parenthesize operand
          else operand
        end
      in
      Ast (formatter operand', precedence)
    in
    format
end
