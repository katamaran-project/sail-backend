open OUnit2


let tests =
  "all tests" >::: [
    ConversionTests.test_suite;
    Substitution_tests.test_suite;
    Precedence_tests.test_suite;
    ExtendedIntegerTests.test_suite;
  ]


let _ = run_test_tt_main tests
