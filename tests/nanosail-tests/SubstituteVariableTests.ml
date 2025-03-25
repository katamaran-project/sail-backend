open Base
open OUnit2
open Nanosail


let mkid    = Ast.Identifier.mk
let evar id = Ast.Expression.Variable (id, Int None)
let svar id = Ast.Statement.Expression (evar id)                


let test_variable =
  let test _ =
    let statement : Ast.Statement.t =
      svar (mkid "x")
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


let test_variable_2 =
  let test _ =
    let statement : Ast.Statement.t =
      svar (mkid "y")
    in
    let substitution (id : Ast.Identifier.t) : Ast.Expression.t option =
      match Ast.Identifier.to_string id with
      | "x" -> Some (Value (Ast.Value.mk_int 1))
      | _   -> None
    in
    let actual : Ast.Statement.t =
      Ast.Statement.substitute_variable substitution statement
    and expected : Ast.Statement.t =
      svar (mkid "y")
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      y [1/x]

    should become

      y
  |} >:: test


let test_binary_operation =
  let test _ =
    let statement : Ast.Statement.t =
      Expression (BinaryOperation (Ast.BinaryOperator.Plus, evar (mkid "x"), evar (mkid "y")))
    in
    let substitution (id : Ast.Identifier.t) : Ast.Expression.t option =
      match Ast.Identifier.to_string id with
      | "x" -> Some (Value (Ast.Value.mk_int 1))
      | _   -> None
    in
    let actual : Ast.Statement.t =
      Ast.Statement.substitute_variable substitution statement
    and expected : Ast.Statement.t =
      Expression (BinaryOperation (Ast.BinaryOperator.Plus, Ast.Expression.Value (Ast.Value.mk_int 1), evar (mkid "y")))
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      (x + y) [1/x]

    should become

      (1 + x)
  |} >:: test


let test_suite =
  "substitute variables" >::: [
    test_variable;
    test_variable_2;
    test_binary_operation;
  ]
