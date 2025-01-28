open OUnit2

module TuplePatternMatchingTests = TuplePatternMatchingTests


let tests =
  "all tests" >::: [
    ConversionTests.test_suite;
    SubstitutionTests.test_suite;
    PrecedenceFormatterTests.test_suite;
    ExtendedIntegerTests.test_suite;
    ConvertBitsToZTests.test_suite;
    NumericExpressionPrettyPrintingTests.test_suite;
    PatternTests.test_suite;
    TuplePatternMatchingTests.test_suite;
  ]


let _ = run_test_tt_main tests
