open Base
open OUnit2
open Nanosail.ExtBase


let test_permutations =
  let tests =
    let test_permutation (xs : int list) (expected : int list list) =
      let actual = Sequence.to_list @@ Sequence.permutations xs
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


let test_take_permutations =
  let tests =
    let test_permutation (xs : int list) (n : int) (expected : int list list) =
      let actual = Sequence.to_list @@ Sequence.take (Sequence.permutations xs) n
      in
      assert_equal ~cmp:(List.equal @@ List.equal Int.equal) expected actual
    in
    let run_test (xs, n, expected) =
      let formatted_xs =
        String.concat ~sep:" " @@ List.map ~f:Int.to_string xs
      in
      Printf.sprintf "permutations of [ %s ]" formatted_xs >:: (fun _ -> test_permutation xs n expected)
    in
    let test_cases =
      [
        (
          [],
          100,
          [
            []
          ]
        );
        (
          [1],
          100,
          [
            [1]
          ]
        );
        (
          [1; 2],
          100,
          [
            [1; 2];
            [2; 1];
          ]
        );
        (
          [1; 2; 3],
          100,
          [
            [1; 2; 3];
            [1; 3; 2];
            [2; 1; 3];
            [2; 3; 1];
            [3; 1; 2];
            [3; 2; 1];
          ]
        );
        (
          [1; 2; 3],
          3,
          [
            [1; 2; 3];
            [1; 3; 2];
            [2; 1; 3];
          ]
        );
        (
          List.range ~stop:`inclusive 1 10,
          4,
          [
            [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];
            [1; 2; 3; 4; 5; 6; 7; 8; 10; 9];
            [1; 2; 3; 4; 5; 6; 7; 9; 8; 10];
            [1; 2; 3; 4; 5; 6; 7; 9; 10; 8];
          ]
        );
        (
          List.range ~stop:`inclusive 1 10000,
          1,
          [
            List.range ~stop:`inclusive 1 10000
          ]
        );
      ]
    in
    List.map ~f:run_test test_cases
  in
  "take permutations" >::: tests


let test_suite =
  "sequence" >::: [
    test_permutations;
    test_take_permutations;
  ]
