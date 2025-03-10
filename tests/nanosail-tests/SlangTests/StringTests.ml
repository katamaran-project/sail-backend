open Nanosail.ExtBase
open OUnit2
open SlangShared

module Slang = Nanosail.Slang


let string_literal_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
           "a"
        |},
        String "a"
      );
      (
        {|
           "b"
        |},
        String "b"
      );
      (
        {|
           "xyz"
        |},
        String "xyz"
      );
      (
        {|
           "\""
        |},
        String {|"|}
      );
      (
        {|
           "\"abc\""
        |},
        String {|"abc"|}
      );
      (
        {|
           "\n"
        |},
        String "\n"
      );
      (
        {|
           "\n"
        |},
        String "\n"
      );
      (
        {|
           "\\"
        |},
        String "\\"
      );
    ]
  in
  "arithmetic" >::: List.map ~f:(Fn.uncurry test_run) test_cases


let string_join_tests =
  let open Slang.Value
  in
  let test_cases =
    [
      (
        {|
           (join " " '("a"))
        |},
        String "a"
      );
      (
        {|
           (join " " '("a" "b"))
        |},
        String "a b"
      );
      (
        {|
           (join " " '("a" "b" "c"))
        |},
        String "a b c"
      );
      (
        {|
           (join " " '("abc" "xyz" "123"))
        |},
        String "abc xyz 123"
      );
      (
        {|
           (join "" '("abc" "xyz" "123"))
        |},
        String "abcxyz123"
      );
      (
        {|
           (join "," '("abc" "xyz" "123"))
        |},
        String "abc,xyz,123"
      );
      (
        {|
           (join ", " '("abc" "xyz" "123"))
        |},
        String "abc, xyz, 123"
      );
    ]
  in
  "arithmetic" >::: List.map ~f:(Fn.uncurry test_run) test_cases



let tests =
  "evaluation tests" >::: [
    string_literal_tests;
    string_join_tests;
  ]
