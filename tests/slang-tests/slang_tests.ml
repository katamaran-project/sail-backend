open OUnit2


let tests =
  "all tests" >::: [
    ContinuationTests.tests;
    TokenizerTests.tests;
    ParserTests.tests;
    EvaluationTests.tests;
    Type_tests.tests;
    AdvancedTests.tests;
    HeapTests.tests;
  ]


let _ = run_test_tt_main tests
