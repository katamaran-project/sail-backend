open Base
open OUnit2
open Nanosail.ExtBase


let test_permutations =
  let tests =
    let test_permutation (xs : int list) (expected : int list list) =
      let actual = List.permutations xs
      in
      assert_equal ~cmp:(List.equal @@ List.equal Int.equal) expected actual
    in
    let run_test (xs, expected) =
      let formatted_xs =
        String.concat ~sep:" " @@ List.map ~f:Int.to_string xs
      in
      Printf.sprintf "permutations of [ %s ]" formatted_xs >:: (fun _ -> test_permutation xs expected)
    in
    let test_cases =
      [
        (
          [],
          [
            []
          ]
        );
        (
          [1],
          [
            [1]
          ]
        );
        (
          [1; 2],
          [
            [1; 2];
            [2; 1];
          ]
        );
        (
          [1; 2; 3],
          [
            [1; 2; 3];
            [1; 3; 2];
            [2; 1; 3];
            [2; 3; 1];
            [3; 1; 2];
            [3; 2; 1];
          ]
        );
      ]
    in
    List.map ~f:run_test test_cases
  in
  "permutations" >::: tests


let test_generate_permutations =
  let tests =
    let test_permutation (xs : int list) (expected : int list list) =
      let results = ref []
      in
      let return_value = List.generate_permutations xs (fun permutation -> results := permutation :: !results; true)
      in
      let actual = List.rev !results
      in
      assert_equal true return_value;
      assert_equal ~cmp:(List.equal @@ List.equal Int.equal) expected actual
    in
    let run_test (xs, expected) =
      let formatted_xs =
        String.concat ~sep:" " @@ List.map ~f:Int.to_string xs
      in
      Printf.sprintf "permutations of [ %s ]" formatted_xs >:: (fun _ -> test_permutation xs expected)
    in
    let test_cases =
      [
        (
          [],
          [
            []
          ]
        );
        (
          [1],
          [
            [1]
          ]
        );
        (
          [1; 2],
          [
            [1; 2];
            [2; 1];
          ]
        );
        (
          [1; 2; 3],
          [
            [1; 2; 3];
            [1; 3; 2];
            [2; 1; 3];
            [2; 3; 1];
            [3; 1; 2];
            [3; 2; 1];
          ]
        );
      ]
    in
    List.map ~f:run_test test_cases
  in
  "generate permutations" >::: tests


let test_suite =
  "list" >::: [
    test_permutations;
    test_generate_permutations;
  ]
