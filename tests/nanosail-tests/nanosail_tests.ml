open OUnit2


let tests =
  "all tests" >::: [
    ConversionTests.test_suite;
    SubstitutionTests.test_suite;
    PrecedenceFormatterTests.test_suite;
    ExtendedIntegerTests.test_suite;
    ConvertBitsToZTests.test_suite;
    NumericExpressionPrettyPrintingTests.test_suite;
    PatternMatchingTests.test_suite;
    NormalizationTests.test_suite;
    SimplificationTests.test_suite;
    ListTests.test_suite;
    RenamingTests.test_suite;
  ]


let _ = run_test_tt_main tests
