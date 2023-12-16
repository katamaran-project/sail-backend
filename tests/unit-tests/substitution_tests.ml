open OUnit2


let test_suite_for_sanitizing_substitution =
  let open Nanosail.Substitute
  in
  let check (input, expected) =
    let test_name =
      Printf.sprintf "sanitizing_substitution %s should return %s" input expected
    in
    test_name >:: fun _ -> begin
        let actual = sanitizing_substitution input
        in
        let message = Printf.sprintf "Expected %s, actual %s" expected actual
        in
        assert_equal ~msg:message expected actual
      end
  in
  let test_cases = [
    ("a"    , "a"  );
    ("'a"   , "a"  );
    ("''xyz", "xyz");
  ]
  in
  "sanitizing_substitution tests" >::: List.map check test_cases
