open OUnit2


let tests =
  "all tests" >::: [
    ContinuationTests.test_suite;
    StateMonadTests.test_suite;
    Writer_monad_tests.test_suite;
    ReaderMonadTests.test_suite;
  ]


let _ = run_test_tt_main tests
