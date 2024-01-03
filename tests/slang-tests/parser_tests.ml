open OUnit2
open Slang.Parser
open Slang.Ast


let parse_string_tests =
  let test_cases =
    [
      ("5", [Integer 5]);
      ("1 2", [Integer 1; Integer 2]);
    ]
  in
  let test_parse_string (input, expected) =
    input >:: fun _ -> begin
        let actual = parse_string input
        in
        assert_equal expected actual
      end
  in
  "parse string tests" >::: List.map test_parse_string test_cases


let tests =
  "paring tests" >::: [
    parse_string_tests;
  ]
