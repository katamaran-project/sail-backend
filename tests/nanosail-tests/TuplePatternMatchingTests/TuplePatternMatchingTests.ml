open OUnit2


let test_suite =
  "tuple pattern matching" >::: [
    BuildChainTests.test_suite;
    CategorizeTests.test_suite;
    BuildStatementTests.test_suite;
  ]
