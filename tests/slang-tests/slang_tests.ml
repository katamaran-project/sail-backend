open OUnit2


let tests =
  "all tests" >::: [
    ContinuationTests.tests;
    Tokenizer_tests.tests;
    Parser_tests.tests;
    Evaluation_tests.tests;
    Type_tests.tests;
    AdvancedTests.tests;
    HeapTests.tests;
  ]


let _ = run_test_tt_main tests
