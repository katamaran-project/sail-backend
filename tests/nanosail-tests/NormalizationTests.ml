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
    List.map ~f:(Fn.compose test_is_generated mkgid) @@ List.range 0 100;
    List.map ~f:(Fn.compose test_is_not_generated mkid) @@ [ "x"; "y"; "xyz" ];
  ]


let test_normalize_expressions =
  let test (expression, expected) =
    let label =
      Printf.sprintf "normalizing %s" (FExpr.to_string @@ Ast.Expression.to_fexpr expression)
    in
    label >:: fun _ -> begin
        let actual = Normalize.normalize_expression expression
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
  let mkgenvar ?(t = Ast.Type.Int None) n =
    Variable (mkgid n, t)
  in
  "expressions" >::: List.map ~f:test [
    (
      Variable (mkid "x", Ast.Type.Int None),
      Variable (mkid "x", Ast.Type.Int None)
    );
    (
      mkgenvar 0,
      mkgenvar 0
    );
    (
      mkgenvar 1,
      mkgenvar 0
    );
    (
      mkgenvar 500,
      mkgenvar 0
    );
    (
      BinaryOperation (Ast.BinaryOperator.And, mkgenvar ~t:Ast.Type.Bool 0, mkgenvar ~t:Ast.Type.Bool 1),
      BinaryOperation (Ast.BinaryOperator.And, mkgenvar ~t:Ast.Type.Bool 0, mkgenvar ~t:Ast.Type.Bool 1)
    );
    (
      BinaryOperation (Ast.BinaryOperator.And, mkgenvar ~t:Ast.Type.Bool 1, mkgenvar ~t:Ast.Type.Bool 2),
      BinaryOperation (Ast.BinaryOperator.And, mkgenvar ~t:Ast.Type.Bool 0, mkgenvar ~t:Ast.Type.Bool 1)
    );
    (
      BinaryOperation (Ast.BinaryOperator.And, mkgenvar ~t:Ast.Type.Bool 5, mkgenvar ~t:Ast.Type.Bool 3),
      BinaryOperation (Ast.BinaryOperator.And, mkgenvar ~t:Ast.Type.Bool 0, mkgenvar ~t:Ast.Type.Bool 1)
    );
    (
      BinaryOperation (Ast.BinaryOperator.And, mkgenvar ~t:Ast.Type.Bool 5, mkgenvar ~t:Ast.Type.Bool 5),
      BinaryOperation (Ast.BinaryOperator.And, mkgenvar ~t:Ast.Type.Bool 0, mkgenvar ~t:Ast.Type.Bool 0)
    );
    (
      Tuple [ mkgenvar 0 ],
      Tuple [ mkgenvar 0 ]
    );
    (
      Tuple [ mkgenvar 1 ],
      Tuple [ mkgenvar 0 ]
    );
    (
      Tuple [ mkgenvar 1; mkgenvar 0 ],
      Tuple [ mkgenvar 0; mkgenvar 1 ]
    );
    (
      Tuple [ mkgenvar 1; mkgenvar 1 ],
      Tuple [ mkgenvar 0; mkgenvar 0 ]
    );
    (
      Tuple [ mkgenvar 5; mkgenvar 5 ],
      Tuple [ mkgenvar 0; mkgenvar 0 ]
    );
    (
      Tuple [ mkgenvar 1; mkgenvar 2; mkgenvar 3; mkgenvar 4 ],
      Tuple [ mkgenvar 0; mkgenvar 1; mkgenvar 2; mkgenvar 3 ]
    );
    (
      Tuple [ mkgenvar 1; mkgenvar 1; mkgenvar 3; mkgenvar 4 ],
      Tuple [ mkgenvar 0; mkgenvar 0; mkgenvar 1; mkgenvar 2 ]
    );
    (
      Tuple [ mkgenvar 1; mkgenvar 1; mkgenvar 3; mkgenvar 1 ],
      Tuple [ mkgenvar 0; mkgenvar 0; mkgenvar 1; mkgenvar 0 ]
    );
  ]


let test_suite =
  "normalization" >::: [
    test_normalize_expressions
  ]
