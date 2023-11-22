open OUnit2
module S = Libsail
module N = Nanosail


let test_process_numeric_expression _context =
  let sail = S.Ast.Nexp_aux (S.Ast.Nexp_constant (Z.of_int 5), Unknown)
  and expected = "5"
  in
  let nano = N.Sail_to_nanosail.translate_numeric_expression sail
  in
  let document = Nanosail.Pretty_printing_katamaran.numeric_expression_pp nano
  in
  let buffer = Stdlib.Buffer.create 1000
  in
  PPrint.ToBuffer.pretty 1.0 1000 buffer document;
  let actual = Stdlib.Buffer.contents buffer
  in
  assert_equal expected actual


let tests =
  let test = "test pretty printing numeric expression" >:: test_process_numeric_expression
  in
  let suite = [ test ]
  in
  "suite" >::: suite


let _ = run_test_tt_main tests
