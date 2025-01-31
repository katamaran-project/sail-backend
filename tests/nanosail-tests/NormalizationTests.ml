open Base
open OUnit2
open Nanosail


let mkid  = Ast.Identifier.mk
let mkgid = Fn.compose Ast.Identifier.mk_generated Int.to_string


let test_is_generated =
  let test_is_generated identifier =
    let test_label =
      FExpr.to_string @@ Ast.Identifier.to_fexpr identifier
    and message =
      "should be recognized as generated"
    in
    test_label >:: fun _ -> assert_bool message (Ast.Identifier.is_generated identifier)
  and test_is_not_generated identifier =
    let test_label =
      FExpr.to_string @@ Ast.Identifier.to_fexpr identifier
    and message =
      "should be recognized as not generated"
    in
    test_label >:: fun _ -> assert_bool message (not @@ Ast.Identifier.is_generated identifier)
  in
  "is normalized" >::: List.concat [
    List.map ~f:test_is_generated [ mkgid 0 ];
    List.map ~f:test_is_not_generated [ mkid "x" ];
  ]      
    

let test_normalize_expressions =
  let test (expression, expected) =
    let label =
      Printf.sprintf "normalizing %s" (FExpr.to_string @@ Ast.Expression.to_fexpr expression)
    in
    label >:: fun _ -> begin
        let actual = Ast.Normalize.normalize_expression expression
        in
        assert_equal
          ~cmp:Ast.Expression.equal
          ~printer:(Fn.compose FExpr.to_string Ast.Expression.to_fexpr)
          expected
          actual
      end
  in
  let open Ast.Expression
  in
  "expressions" >::: List.map ~f:test [
    (
      Variable (mkid "x", Ast.Type.Int),
      Variable (mkid "x", Ast.Type.Int)
    );
    (
      Variable (mkgid 0, Ast.Type.Int),
      Variable (mkgid 0, Ast.Type.Int)
    );
    (
      Variable (mkgid 1, Ast.Type.Int),
      Variable (mkgid 0, Ast.Type.Int)
    );
    (
      BinaryOperation (Ast.BinaryOperator.And, Variable (mkgid 0, Ast.Type.Bool), Variable (mkgid 1, Ast.Type.Bool)),
      BinaryOperation (Ast.BinaryOperator.And, Variable (mkgid 0, Ast.Type.Bool), Variable (mkgid 1, Ast.Type.Bool))
    );
    (
      BinaryOperation (Ast.BinaryOperator.And, Variable (mkgid 1, Ast.Type.Bool), Variable (mkgid 2, Ast.Type.Bool)),
      BinaryOperation (Ast.BinaryOperator.And, Variable (mkgid 0, Ast.Type.Bool), Variable (mkgid 1, Ast.Type.Bool))
    );
  ]


let test_suite =
  "normalization" >::: [
    test_normalize_expressions
  ]
