open Base
open OUnit2
open Nanosail


(*
   Loads a configuration from a temporary file. Configuration settings are global,
   so the names used here are chosen not to occur in other tests.
*)
let load_configuration (contents : string) : unit =
  let path = Stdlib.Filename.temp_file "ignore-by-name" ".lisp"
  in
  Stdio.Out_channel.write_all path ~data:contents;
  Configuration.load_configuration path;
  Stdlib.Sys.remove path


let test_ignore_by_name =
  "ignore-functions and ignore-values" >:: fun _ -> begin
      load_configuration {|
        (ignore-values '("ibn_value_1"))
        (ignore-functions '("ibn_function"))
        (ignore-value-definition-predicate (lambda (identifier) (= identifier "ibn_value_by_predicate")))
        (ignore-values '("ibn_value_2" "ibn_value_3"))
      |};
      let check (description : string) (expected : bool) (actual : bool) : unit =
        assert_equal ~printer:Bool.to_string ~msg:description expected actual
      in
      (* names given in several calls add up, and the predicate, set later, does not replace them *)
      check "first list"          true  (Configuration.should_ignore_value_definition "ibn_value_1");
      check "second list"         true  (Configuration.should_ignore_value_definition "ibn_value_3");
      check "predicate"           true  (Configuration.should_ignore_value_definition "ibn_value_by_predicate");
      check "other value"         false (Configuration.should_ignore_value_definition "ibn_value_4");
      (* ignore-functions covers a function's definition and its top-level type constraint, not values *)
      check "function"            true  (Configuration.should_ignore_function_definition "ibn_function");
      check "type constraint"     true  (Configuration.should_ignore_top_level_type_constraint "ibn_function");
      check "function as value"   false (Configuration.should_ignore_value_definition "ibn_function");
      check "value as function"   false (Configuration.should_ignore_function_definition "ibn_value_1");
    end


let test_suite =
  "ignore by name" >::: [
    test_ignore_by_name;
  ]
