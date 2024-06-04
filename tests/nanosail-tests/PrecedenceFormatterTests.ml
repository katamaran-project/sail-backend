open Base
open OUnit2

module PF = Nanosail.PrecedenceFormatter

module StringOutput : PF.Output with type t = string = struct
  type t = string

  let parenthesize = Printf.sprintf "(%s)"
end

module Prec = struct
  include PF.Make(StringOutput)

  let int = Fn.compose define_atom Int.to_string
  let add = define_left_associative_binary_operator  10 (fun x y -> Printf.sprintf "%s + %s" x y)
  let sub = define_left_associative_binary_operator  10 (fun x y -> Printf.sprintf "%s - %s" x y)
  let mul = define_left_associative_binary_operator  20 (fun x y -> Printf.sprintf "%s * %s" x y)
  let div = define_left_associative_binary_operator  20 (fun x y -> Printf.sprintf "%s / %s" x y)
  let pow = define_right_associative_binary_operator 30 (fun x y -> Printf.sprintf "%s ^ %s" x y)
  let neg = define_unary_prefix_operator             50 (fun x   -> Printf.sprintf "-%s"     x  )
  
  let to_string = output_of
end

let test_formatting =
  let test (ast, expected) =
    let f _ =
      let actual = Prec.to_string ast
      in
      let msg    = Printf.sprintf "Expected %s, got %s" expected actual
      and cmp    = String.equal
      in
      OUnit2.assert_equal ~msg ~cmp expected actual
    in
    expected >:: f
  in
  let test_cases =
    let open Prec in
    [
      (int 1, "1");
      (int 5, "5");
      (add (int 1) (int 2), "1 + 2");
      (add (int 1) (add (int 2) (int 3)), "1 + (2 + 3)");
      (add (add (int 1) (int 2)) (int 3), "1 + 2 + 3");
      (sub (int 4) (int 2), "4 - 2");
      (mul (int 1) (int 2), "1 * 2");
      (div (int 8) (int 4), "8 / 4");
      (mul (add (int 1) (int 2)) (int 3), "(1 + 2) * 3");
      (div (add (int 1) (int 2)) (int 3), "(1 + 2) / 3");
      (mul (add (int 1) (int 2)) (add (int 3) (int 4)), "(1 + 2) * (3 + 4)");
      (sub (int 3) (sub (int 2) (int 1)), "3 - (2 - 1)");
      (pow (int 1) (pow (int 2) (int 3)), "1 ^ 2 ^ 3");
      (pow (pow (int 1) (int 2)) (int 3), "(1 ^ 2) ^ 3");
      (neg (int 1), "-1");
      (neg (neg (int 1)), "--1");
      (neg (add (int 1) (int 2)), "-(1 + 2)");
      (neg (div (int 3) (int 2)), "-(3 / 2)");
      (neg (pow (int 3) (int 2)), "-(3 ^ 2)");
    ]
  in
  "formatting" >::: List.map ~f:test test_cases

let test_suite =
  "precedence formatter test suite" >::: [
      test_formatting
    ]

