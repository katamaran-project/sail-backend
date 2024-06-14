open Base
open OUnit2

module EI = Nanosail.ExtendedInteger
module ExtendedIntegerNotations = Nanosail.ComparisonNotations.Make(Nanosail.ExtendedInteger)


let test_comparisons =
  let open EI
  in
  let open ExtendedIntegerNotations
  in
  let assert_lt x y =
    let message = Printf.sprintf "%s < %s" (EI.to_string x) (EI.to_string y)
    in
    message >:: fun _ -> assert_bool message (x << y)
  and assert_le x y =
    let message = Printf.sprintf "%s <= %s" (EI.to_string x) (EI.to_string y)
    in
    message >:: fun _ -> assert_bool message (x <<= y)
  and assert_gt x y =
    let message = Printf.sprintf "%s > %s" (EI.to_string x) (EI.to_string y)
    in
    message >:: fun _ -> assert_bool message (x >> y)
  and assert_ge x y =
    let message = Printf.sprintf "%s >= %s" (EI.to_string x) (EI.to_string y)
    in
    message >:: fun _ -> assert_bool message (x >>= y)
  in
  [
    assert_lt (Int 1) (Int 2);
    assert_lt (Int 1) (Int 3);
    assert_lt (Int 1) PositiveInfinity;
    assert_lt NegativeInfinity (Int 5);
    assert_lt NegativeInfinity PositiveInfinity;
    assert_le (Int 1) (Int 2);
    assert_le (Int 2) (Int 2);
    assert_le (Int 1) PositiveInfinity;
    assert_le PositiveInfinity PositiveInfinity;
    assert_le NegativeInfinity PositiveInfinity;
    assert_le NegativeInfinity NegativeInfinity;
    assert_le NegativeInfinity (Int 9);
    assert_gt (Int 2) (Int 1);
    assert_gt PositiveInfinity (Int 1);
    assert_gt PositiveInfinity NegativeInfinity;
    assert_ge (Int 2) (Int 1);
    assert_ge PositiveInfinity (Int 1);
    assert_ge PositiveInfinity NegativeInfinity;
    assert_ge PositiveInfinity PositiveInfinity;
    assert_ge NegativeInfinity NegativeInfinity;
    assert_ge (Int 3) (Int 3);
  ]


let test_suite =
  "extended integer test suite" >::: test_comparisons

