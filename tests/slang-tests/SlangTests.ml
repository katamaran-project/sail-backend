open OUnit2


let tests =
  "all tests" >::: [
    TokenizerTests.tests;
    ParserTests.tests;
    EvaluationTests.tests;
    TypeTests.tests;
    AdvancedTests.tests;
    HeapTests.tests;
    StringTests.tests;
  ]


let _ = run_test_tt_main tests
