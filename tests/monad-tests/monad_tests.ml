open OUnit2


let tests =
  "all tests" >::: [
    State_monad_tests.test_suite;
    Writer_monad_tests.test_suite;
    Reader_monad_tests.test_suite;
  ]


let _ = run_test_tt_main tests
