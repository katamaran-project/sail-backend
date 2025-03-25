open Base
open OUnit2
open Nanosail


let mkid  = Ast.Identifier.mk


let test_variable =
  let test _ =
    let statement : Ast.Statement.t =
      Expression (Variable (mkid "x", Int None))
    in
    let substitution (id : Ast.Identifier.t) : Ast.Expression.t option =
      match Ast.Identifier.to_string id with
      | "x" -> Some (Value (Ast.Value.mk_int 1))
      | _   -> None
    in
    let actual : Ast.Statement.t =
      Ast.Statement.substitute_variable substitution statement
    and expected : Ast.Statement.t =
      Expression (Value (Ast.Value.mk_int 1))
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      x [1/x]

    should become

      1
  |} >:: test


let test_suite =
  "substitute variables" >::: [
    test_variable
  ]
