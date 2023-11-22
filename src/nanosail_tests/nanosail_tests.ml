open OUnit2
module S = Libsail
module N = Nanosail


let string_of_document ?(line_width = 10000) (document : PPrint.document) : string =
  let buffer = Stdlib.Buffer.create 1000
  in
  PPrint.ToBuffer.pretty 1.0 line_width buffer document;
  Stdlib.Buffer.contents buffer


let addloc (numexp : S.Ast.nexp_aux) =
  S.Ast.Nexp_aux (numexp, Unknown)


let string_of_numeric_expression (nexp : S.Ast.nexp)  =
  let nano_nexp = N.Sail_to_nanosail.translate_numeric_expression nexp in
  let document = Nanosail.Pretty_printing_katamaran.numeric_expression_pp nano_nexp
  in
  string_of_document document



let numeric_expression_test_suite =
  let test_string_of_numeric_expression input expected =
    let test_name =
      Printf.sprintf "Expected %s" expected
    in
    let actual = string_of_numeric_expression input
    in
    test_name >:: fun _ -> assert_equal expected actual
  in
  let const c = addloc (S.Ast.Nexp_constant (Z.of_int c))
  and add x y = addloc (S.Ast.Nexp_sum (x, y))
  and _sub x y = addloc (S.Ast.Nexp_minus (x, y))
  and _mul x y = addloc (S.Ast.Nexp_times (x, y))
  in
  let inputs = [
    (const 5, "5");
    (const 10, "10");
    (add (const 1) (const 2), "1 + 2");
  ]
  in
  "string_of_numeric_expression tests" >::: List.map (fun (input, expected) -> test_string_of_numeric_expression input expected) inputs


let tests =
  "all tests" >::: [ numeric_expression_test_suite ]


let _ = run_test_tt_main tests
