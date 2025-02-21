open OUnit2
open Nanosail


module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match

open PatternMatchingShared
open Monads.Notations.Star(TC)


let test_build_pattern_tree_enum_single_case =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"]
      in
      let* actual_tree =
        build_empty_pattern_tree [ enum_type ]
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = gen#wildcard;
          subtree      = Leaf None
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc
  in
  {|
    enum A = { A1, A2 }

    tuple (A)
  |} >:: test


let test_build_pattern_tree_enum_1 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let* actual_tree =
        build_empty_pattern_tree [ enum_type ]
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = gen#wildcard;
          subtree      = Leaf None
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc
  in
  {|
    enum A = { A1, A2 }

    tuple (A)
  |} >:: test


let test_build_pattern_tree_enum_2 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"; "A3"]
      in
      let* actual_tree =
        build_empty_pattern_tree [ enum_type ]
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = gen#wildcard;
          subtree      = Leaf None
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc
  in
  {|
    enum A = { A1, A2, A3 }

    tuple (A)
  |} >:: test


let test_build_pattern_tree_enum_3 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"]
      in
      let* actual_tree =
        build_empty_pattern_tree [ enum_type; enum_type ]
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = gen#wildcard;
          subtree      = Binder {
              matched_type = enum_type;
              binder       = gen#wildcard;
              subtree      = Leaf None
            }
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1 }

      tuple (A, A)
  |} >:: test


let test_build_pattern_tree_enum_4 =
  let test _ =
    let gen = new generator
    in
    let tc =
      let* enum_type =
        TC.define_enum_str "A" ["A1"; "A2"]
      in
      let* actual_tree =
        build_empty_pattern_tree [ enum_type; enum_type ]
      in
      let expected_tree : TM.PatternTree.t =
        Binder {
          matched_type = enum_type;
          binder       = gen#wildcard;
          subtree      = Binder {
              matched_type = enum_type;
              binder       = gen#wildcard;
              subtree      = Leaf None
            }
        }
      in
      TC.assert_equal_pattern_trees expected_tree actual_tree
    in
    TC.run_expecting_success tc
  in
  {|
      enum A = { A1, A2 }

      tuple (A, A)
  |} >:: test


let test_suite = "enum" >::: [
    test_build_pattern_tree_enum_single_case;
    test_build_pattern_tree_enum_1;
    test_build_pattern_tree_enum_2;
    test_build_pattern_tree_enum_3;
    test_build_pattern_tree_enum_4;
]
