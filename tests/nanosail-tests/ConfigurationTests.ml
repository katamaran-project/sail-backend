open Base
open OUnit2
open Nanosail


let check_bool (description : string) (expected : bool) (actual : bool) : unit =
  assert_equal ~printer:Bool.to_string ~msg:description expected actual


(*
   Writes the files (name, contents) to a fresh directory and returns its path.
   Configuration settings are global, so the names used in these tests are chosen not to occur in other tests.
*)
let write_files (files : (string * string) list) : string =
  let directory = Stdlib.Filename.temp_dir "configuration-tests" ""
  in
  List.iter files ~f:(fun (name, contents) -> Stdio.Out_channel.write_all (Stdlib.Filename.concat directory name) ~data:contents);
  directory


let load_configuration (contents : string) : unit =
  let directory = write_files [ ("configuration.sexp", contents) ]
  in
  Configuration.load_configuration (Stdlib.Filename.concat directory "configuration.sexp")


(* The message of the Failure that f raises *)
let failure_message (f : unit -> unit) : string =
  match f () with
  | ()                     -> assert_failure "expected a failure"
  | exception Failure text -> text


let test_patterns =
  "patterns" >:: fun _ -> begin
      let matches pattern name = NameFilter.pattern_matches ~pattern name
      in
      check_bool "exact"                 true  (matches "log2" "log2");
      check_bool "exact, other name"     false (matches "log2" "log");
      check_bool "prefix"                true  (matches "num_of_*" "num_of_ExceptionType");
      check_bool "prefix, other name"    false (matches "num_of_*" "of_num");
      check_bool "suffix"                true  (matches "*_of_num" "Privilege_of_num");
      check_bool "suffix, exactly"       true  (matches "*_of_num" "_of_num");
      check_bool "infix"                 true  (matches "*_of_*" "num_of_x");
      check_bool "two stars in a row"    true  (matches "a**b" "ab");
      check_bool "parts in order"        true  (matches "a*b*c" "axbyc");
      check_bool "parts out of order"    false (matches "a*b*c" "acb");
      check_bool "overlapping ends"      false (matches "ab*ba" "aba");
      check_bool "everything"            true  (matches "*" "anything");
      check_bool "empty name"            true  (matches "*" "")
    end


let test_name_filter =
  "name filters" >:: fun _ -> begin
      let filter =
        NameFilter.union
          (NameFilter.of_patterns ~patterns:[ "num_of_*"; "log2" ] ~exceptions:[ "num_of_ExceptionType" ])
          (NameFilter.of_patterns ~patterns:[ "num_of_ExceptionType_listed" ] ~exceptions:[])
      in
      check_bool "pattern"                         true  (NameFilter.matches filter "num_of_Privilege");
      check_bool "exception"                       false (NameFilter.matches filter "num_of_ExceptionType");
      check_bool "exception is local to its list"  true  (NameFilter.matches filter "num_of_ExceptionType_listed");
      check_bool "exact name"                      true  (NameFilter.matches filter "log2");
      check_bool "other name"                      false (NameFilter.matches filter "log");
      check_bool "empty filter"                    false (NameFilter.matches NameFilter.empty "log2");
      check_bool "everything"                      true  (NameFilter.matches NameFilter.everything "log2")
    end


let test_parsing =
  "parsing" >:: fun _ -> begin
      let nodes = ConfigurationFile.parse_string ~file:"test.sexp" "; a comment\n(option plain \"quoted \\\"text\\\"\")\n#;(ignored)\n(second)"
      in
      match nodes with
      | [ List { elements = [ Atom option; Atom plain; Atom quoted ]; location }; List { location = second_location; _ } ] -> begin
          assert_equal ~printer:Fn.id "option" option.text;
          check_bool "plain atom is not quoted" false plain.quoted;
          check_bool "quoted atom is quoted"    true  quoted.quoted;
          assert_equal ~printer:Fn.id "quoted \"text\"" quoted.text;
          assert_equal ~printer:Int.to_string 2 location.line;
          assert_equal ~printer:Int.to_string 4 second_location.line
        end
      | _ -> assert_failure "unexpected forms"
    end


let test_ignore_by_name =
  "ignore" >:: fun _ -> begin
      load_configuration {|
        (ignore (values cfg_value_1))
        (ignore (functions cfg_function "cfg_pattern_*" (except cfg_pattern_kept)))
        (ignore (function-definitions cfg_definition_only)
                (type-constraints cfg_constraint_only))
        (ignore (values cfg_value_2 cfg_value_3))
      |};
      (* names given in several forms add up *)
      check_bool "first list"            true  (Configuration.should_ignore_value_definition "cfg_value_1");
      check_bool "second list"           true  (Configuration.should_ignore_value_definition "cfg_value_3");
      check_bool "other value"           false (Configuration.should_ignore_value_definition "cfg_value_4");
      (* functions covers a function's definition and its top-level type constraint, not values *)
      check_bool "function"              true  (Configuration.should_ignore_function_definition "cfg_function");
      check_bool "type constraint"       true  (Configuration.should_ignore_top_level_type_constraint "cfg_function");
      check_bool "function as value"     false (Configuration.should_ignore_value_definition "cfg_function");
      check_bool "value as function"     false (Configuration.should_ignore_function_definition "cfg_value_1");
      check_bool "pattern"               true  (Configuration.should_ignore_function_definition "cfg_pattern_x");
      check_bool "exception"             false (Configuration.should_ignore_function_definition "cfg_pattern_kept");
      (* function-definitions and type-constraints select one of the two *)
      check_bool "definition only"       true  (Configuration.should_ignore_function_definition "cfg_definition_only");
      check_bool "not its constraint"    false (Configuration.should_ignore_top_level_type_constraint "cfg_definition_only");
      check_bool "constraint only"       true  (Configuration.should_ignore_top_level_type_constraint "cfg_constraint_only");
      check_bool "not its definition"    false (Configuration.should_ignore_function_definition "cfg_constraint_only")
    end


let test_include =
  "include" >:: fun _ -> begin
      let directory =
        write_files [
          ("main.sexp"  , "(include shared.sexp)\n(ignore (values cfg_main))");
          ("shared.sexp", "(ignore (values cfg_included))");
        ]
      in
      Configuration.load_configuration (Stdlib.Filename.concat directory "main.sexp");
      check_bool "included"   true (Configuration.should_ignore_value_definition "cfg_included");
      check_bool "main"       true (Configuration.should_ignore_value_definition "cfg_main")
    end


let test_errors =
  "errors" >:: fun _ -> begin
      let contains message part = check_bool (Printf.sprintf "%S contains %S" message part) true (String.is_substring message ~substring:part)
      in
      let unknown = failure_message (fun () -> load_configuration "(ignore (values cfg_x))\n\n(no-such-option)")
      in
      contains unknown "configuration.sexp:3";
      contains unknown "unknown option no-such-option";
      let unknown_part = failure_message (fun () -> load_configuration "(ignore (fuctions cfg_y))")
      in
      contains unknown_part "unknown part (fuctions ...) of ignore";
      let cycle =
        let directory = write_files [ ("a.sexp", "(include b.sexp)"); ("b.sexp", "(include a.sexp)") ]
        in
        failure_message (fun () -> Configuration.load_configuration (Stdlib.Filename.concat directory "a.sexp"))
      in
      contains cycle "includes itself";
      let template = failure_message (fun () -> load_configuration "(templates base.v)")
      in
      contains template "must contain \".template\""
    end


let test_suite =
  "configuration" >::: [
    test_patterns;
    test_name_filter;
    test_parsing;
    test_ignore_by_name;
    test_include;
    test_errors;
  ]
