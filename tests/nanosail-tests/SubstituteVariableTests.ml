open Base
open OUnit2
open Nanosail


let mkid    = Ast.Identifier.mk
let evar id = Ast.Expression.Variable (id, Int None)
let svar id = Ast.Statement.Expression (evar id)                
let eval n  = Ast.Expression.Value (Ast.Value.mk_int n)


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
      Expression (eval 1)
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
      | "x" -> Some (eval 1)
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
      | "x" -> Some (eval 1)
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


let test_binary_operation_2 =
  let test _ =
    let statement : Ast.Statement.t =
      Expression (BinaryOperation (Ast.BinaryOperator.Plus, evar (mkid "x"), evar (mkid "y")))
    in
    let substitution (id : Ast.Identifier.t) : Ast.Expression.t option =
      match Ast.Identifier.to_string id with
      | "y" -> Some (eval 1)
      | _   -> None
    in
    let actual : Ast.Statement.t =
      Ast.Statement.substitute_variable substitution statement
    and expected : Ast.Statement.t =
      Expression (BinaryOperation (Ast.BinaryOperator.Plus, evar (mkid "x"), Ast.Expression.Value (Ast.Value.mk_int 1)))
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      (x + y) [1/y]

    should become

      (x + 1)
  |} >:: test


let test_binary_operation_3 =
  let test _ =
    let statement : Ast.Statement.t =
      Expression (BinaryOperation (Ast.BinaryOperator.Plus, evar (mkid "x"), evar (mkid "y")))
    in
    let substitution (id : Ast.Identifier.t) : Ast.Expression.t option =
      match Ast.Identifier.to_string id with
      | "x" -> Some (eval 1)
      | "y" -> Some (eval 2)
      | _   -> None
    in
    let actual : Ast.Statement.t =
      Ast.Statement.substitute_variable substitution statement
    and expected : Ast.Statement.t =
      Expression (BinaryOperation (Ast.BinaryOperator.Plus, Ast.Expression.Value (Ast.Value.mk_int 1), Ast.Expression.Value (Ast.Value.mk_int 2)))
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      (x + y) [1/x][2/y]

    should become

      (1 + 2)
  |} >:: test


let test_let =
  let test _ =
    let statement : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Int None;
        binding_statement      = svar (mkid "y");
        body_statement         = svar (mkid "z");
      }
    in
    let substitution (id : Ast.Identifier.t) : Ast.Expression.t option =
      match Ast.Identifier.to_string id with
      | "x" -> Some (eval 1)
      | _   -> None
    in
    let actual : Ast.Statement.t =
      Ast.Statement.substitute_variable substitution statement
    and expected : Ast.Statement.t =
      Let {
        binder                 = mkid "x";
        binding_statement_type = Int None;
        binding_statement      = svar (mkid "y");
        body_statement         = svar (mkid "z");
      }
    in
    assert_equal
      ~cmp:Ast.Statement.equal
      ~printer:(Fn.compose FExpr.to_string Ast.Statement.to_fexpr)
      expected
      actual
  in
  {|
      (let x = y in z) [1/x]

    should become

      let x = y in z
  |} >:: test


let test_suite =
  "substitute variables" >::: [
    test_variable;
    test_variable_2;
    test_binary_operation;
    test_binary_operation_2;
    test_binary_operation_3;
    test_let;
  ]
