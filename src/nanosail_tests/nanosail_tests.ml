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


let test_process_numeric_expression _context =
  let nexp = S.Ast.Nexp_constant (Z.of_int 5)
  and expected = "5"
  in
  let actual = string_of_numeric_expression nexp
  in
  assert_equal expected actual


let tests =
  let test = "test pretty printing numeric expression" >:: test_process_numeric_expression
  in
  let suite = [ test ]
  in
  "suite" >::: suite


let _ = run_test_tt_main tests
