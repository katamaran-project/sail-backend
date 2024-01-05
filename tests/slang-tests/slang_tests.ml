open OUnit2


let tests =
  "all tests" >::: [
    Continuation_tests.tests;
    Tokenizer_tests.tests;
    Parser_tests.tests;
    Evaluation_tests.tests;
  ]


let _ = run_test_tt_main tests
