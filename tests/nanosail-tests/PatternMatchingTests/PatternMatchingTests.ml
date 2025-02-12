open OUnit2

module AdornTests          = AdornTests
module BuildStatementTests = BuildStatementTests
module BuildTreeTests      = BuildTreeTests


let test_suite =
  "pattern matching" >::: [
    BuildTreeTests.test_suite;
    AdornTests.test_suite;
    BuildStatementTests.test_suite;
  ]
