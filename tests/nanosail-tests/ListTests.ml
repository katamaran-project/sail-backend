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
        )
      ]
    in
    List.map ~f:run_test test_cases
  in
  "permutations" >::: tests


let test_suite =
  "list" >::: [
    test_permutations
  ]
