open Nanosail.ExtBase
open OUnit2


let converts_bits_to_z = Nanosail.Util.convert_bits_to_z


let test_convert_bits_to_z =
  let check (bits : int list) (expected : Z.t) =
    let test_name =
      let formatted_bits = Nanosail.StringOf.OCaml.list Int.to_string bits
      in
      Printf.sprintf "converting bits %s should return %s" formatted_bits (Z.to_string expected)
    in
    let bits_as_bools : bool list =
      List.map ~f:(fun x -> Int.equal x 1) bits
    in
    test_name >:: fun _ -> begin
        let actual = converts_bits_to_z bits_as_bools
        in
        let message = Printf.sprintf "Expected %s, actual %s" (Z.to_string expected) (Z.to_string actual)
        in
        assert_equal ~msg:message expected actual

      end
  in
  let test_cases = [
    ([0], Z.of_int 0);
    ([1], Z.of_int 1);
    ([1; 0], Z.of_int 2);
    ([1; 1], Z.of_int 3);
    ([1; 0; 0], Z.of_int 4);
    ([1; 0; 1], Z.of_int 5);
    ([1; 1; 0], Z.of_int 6);
    ([1; 1; 1], Z.of_int 7);
    ([1;1;1;0;0;0;0;1;0;1;1;0;1;0;0;0;0;0;1;0;0], Z.of_int 1846532);
  ]
  in
  "convert bits to z tests" >::: List.map ~f:(Fn.uncurry check) test_cases


let test_suite =
  "substitution test suite" >::: [
    test_convert_bits_to_z;
  ]
