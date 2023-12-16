open OUnit2


let tests =
  "all tests" >::: [
    Conversion_tests.test_suite;
    Substitution_tests.test_suite;
    State_monad_tests.test_suite;
    Writer_monad_tests.test_suite;
  ]


let _ = run_test_tt_main tests
