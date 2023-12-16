open OUnit2


let tests =
  "all tests" >::: [
    Conversion_tests.test_suite_numeric_expression;
    Substitution_tests.test_suite_for_sanitizing_substitution;
  ]


let _ = run_test_tt_main tests
