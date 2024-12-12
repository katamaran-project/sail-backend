open OUnit2

module Slang = Nanosail.Slang
open Slang.Parser
open Slang.Value


let rec make_list xs =
  match xs with
  | []    -> Nil
  | x::xs -> Cons (x, make_list xs)


let parse_string_tests =
  let test_cases =
    [
      ("5", [Integer 5]);
      ("1 2", [Integer 1; Integer 2]);
      ("(1 2)", [Cons (Integer 1, Cons (Integer 2, Nil))]);
      ("()", [Nil]);
      ("(* 5 (1 2))", [
         make_list [
             Symbol "*";
             Integer 5;
             make_list [ Integer 1; Integer 2 ]
           ]
      ]);
      ("'5", [
         make_list [
             Symbol "quote";
             Integer 5;
           ]
      ]);
      ("(a '(b c) d)", [
          make_list [
            Symbol "a";
            make_list [
              Symbol "quote";
              make_list [
                Symbol "b";
                Symbol "c";
              ];
            ];
            Symbol "d";
          ]
        ]);
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
