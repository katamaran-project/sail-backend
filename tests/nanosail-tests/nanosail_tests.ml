open OUnit2


let tests =
  "all tests" >::: [
    ConversionTests.test_suite;
    SubstitutionTests.test_suite;
    PrecedenceTests.test_suite;
    ExtendedIntegerTests.test_suite;
  ]


let _ = run_test_tt_main tests
