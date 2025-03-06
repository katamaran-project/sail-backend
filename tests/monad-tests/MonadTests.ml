open OUnit2


let tests =
  "all tests" >::: [
    StateMonadTests.test_suite;
    WriterMonadTests.test_suite;
    DispenserMonadTests.test_suite;
  ]


let _ = run_test_tt_main tests
