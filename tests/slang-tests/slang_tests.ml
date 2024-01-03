open OUnit2


(* let test_parse_sexpression string expected = *)
(*   let actual = Slang.parse string *)
(*   in *)
(*   assert_equal expected actual *)


let tests =
  "all tests" >::: [
    Continuation_tests.tests;
    Tokenizer_tests.tests;
  ]


let _ = run_test_tt_main tests
