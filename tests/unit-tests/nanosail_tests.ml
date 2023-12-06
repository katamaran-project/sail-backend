open OUnit2
module S = Libsail



let string_of_document ?(line_width = 10000) (document : PPrint.document) : string =
  let buffer = Stdlib.Buffer.create 1000
  in
  PPrint.ToBuffer.pretty 1.0 line_width buffer document;
  Stdlib.Buffer.contents buffer


let addloc (numexp : S.Ast.nexp_aux) =
  S.Ast.Nexp_aux (numexp, Unknown)


let string_of_numeric_expression (nexp : S.Ast.nexp)  =
  let nano_nexp = Nanosail.translate_numeric_expression nexp in
  let document =
    Nanosail.Gen.Monad.run_result (Nanosail.Gen.Sail.pp_numeric_expression nano_nexp)
  in
  string_of_document document


let numeric_expression_test_suite =
  let test_string_of_numeric_expression input expected =
    let test_name =
      Printf.sprintf "Expected %s" expected
    in
    test_name >:: fun _ -> (
        let actual = string_of_numeric_expression input
        in
        let message = Printf.sprintf "Expected %s, actual %s" expected actual
        in
        assert_equal ~msg:message expected actual
      )
  in
  let const c = addloc (S.Ast.Nexp_constant (Z.of_int c))
  and add x y = addloc (S.Ast.Nexp_sum (x, y))
  and sub x y = addloc (S.Ast.Nexp_minus (x, y))
  and mul x y = addloc (S.Ast.Nexp_times (x, y))
  and neg x = addloc (S.Ast.Nexp_neg x)
  in
  let inputs = [
    (const 5, "5");
    (const 10, "10");
    (add (const 1) (const 2), "1 + 2");
    (sub (const 3) (const 1), "3 - 1");
    (mul (const 2) (const 4), "2 * 4");
    (mul (const 2) (add (const 3) (const 4)), "2 * (3 + 4)");
    (neg (const 1), "-1");
    (neg (add (const 1) (const 2)), "-(1 + 2)");
    (neg (mul (const 1) (const 2)), "-(1 * 2)");
  ]
  in
  "string_of_numeric_expression tests" >::: List.map (fun (input, expected) -> test_string_of_numeric_expression input expected) inputs


let tests =
  "all tests" >::: [ numeric_expression_test_suite ]


let _ = run_test_tt_main tests
