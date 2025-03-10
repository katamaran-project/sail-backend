open Nanosail
open OUnit2

module Shared = Shared

let tests =
  "all tests" >::: [
    SlangTests.test_suite;
    ConversionTests.test_suite;
    PrecedenceFormatterTests.test_suite;
    ExtendedIntegerTests.test_suite;
    ConvertBitsToZTests.test_suite;
    NumericExpressionPrettyPrintingTests.test_suite;
    PatternMatchingTests.test_suite;
    NormalizationTests.test_suite;
    SimplificationTests.test_suite;
    ListTests.test_suite;
    SequenceTests.test_suite;
    RenamingTests.test_suite;
    BlockTests.test_suite;
  ]


(* Set verbosity level to quiet unless user explicitly specified it using an environment variable *)
let _ =
  match Sys.getenv_opt Configuration.verbosity_level_environment_variable with
  | None   -> Configuration.(set verbosity_level) LogLib.VerbosityLevel.quiet
  | Some _ -> ()


let _ = run_test_tt_main tests
