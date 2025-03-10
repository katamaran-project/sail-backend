open OUnit2


let test_suite =
  "slang tests" >::: [
    TokenizerTests.tests;
    ParserTests.tests;
    EvaluationTests.tests;
    TypeTests.tests;
    AdvancedTests.tests;
    HeapTests.tests;
    StringTests.tests;
  ]
