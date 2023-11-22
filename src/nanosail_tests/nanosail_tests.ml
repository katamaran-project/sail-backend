open OUnit2
module S = Libsail
module N = Nanosail


let string_of_document ?(line_width = 10000) (document : PPrint.document) : string =
  let buffer = Stdlib.Buffer.create 1000
  in
  PPrint.ToBuffer.pretty 1.0 line_width buffer document;
  Stdlib.Buffer.contents buffer


let string_of_numeric_expression (numeric_expression : S.Ast.nexp_aux)  =
  let nexp = S.Ast.Nexp_aux (numeric_expression, Unknown) in
  let nano_nexp = N.Sail_to_nanosail.translate_numeric_expression nexp in
  let document = Nanosail.Pretty_printing_katamaran.numeric_expression_pp nano_nexp
  in
  string_of_document document



let numeric_expression_test_suite =
  let test_string_of_numeric_expression input expected _context =
    let actual = string_of_numeric_expression input
    in
    assert_equal expected actual
  in
  let inputs = [
    (S.Ast.Nexp_constant (Z.of_int 5), "5")
  ]
  in
  "string_of_numeric_expression tests" >::: List.map (fun (input, expected) -> "testing string_of_numeric_expression" >:: test_string_of_numeric_expression input expected) inputs


let tests =
  "all tests" >::: [ numeric_expression_test_suite ]


let _ = run_test_tt_main tests
