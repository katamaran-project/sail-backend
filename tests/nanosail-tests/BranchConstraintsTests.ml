open Base
open OUnit2

module S = Libsail.Ast
module U = Libsail.Ast_util
module TypeCheck = Libsail.Type_check
module KidSet = Libsail.Ast_compare.KidSet
module BranchConstraints = Nanosail.SailToNanosail.Translate.BranchConstraints


let parse_constraint (source : string) : S.n_constraint =
  Libsail.Initial_check.constraint_of_string Libsail.Initial_check.initial_ctx source

let kid (name : string) : S.kid =
  U.mk_kid name

let kids_to_string (kids : KidSet.t) : string =
  String.concat ~sep:", " @@ List.map ~f:U.string_of_kid @@ KidSet.elements kids

let constraints_to_string (constraints : S.n_constraint list) : string =
  String.concat ~sep:", " @@ List.map ~f:U.string_of_n_constraint constraints


(*
   Tests on constraint lists alone: which constraints count as added,
   and which size variables are linked to them.
*)
let test_added_constraints =
  let test (name : string) ~(outer : string list) ~(inner : string list) ~(expected : string list) =
    name >:: fun _ ->
      let actual = BranchConstraints.added_constraints ~outer:(List.map ~f:parse_constraint outer) ~inner:(List.map ~f:parse_constraint inner)
      in
      assert_equal ~printer:Fn.id (constraints_to_string @@ List.map ~f:parse_constraint expected) (constraints_to_string actual)
  in
  "added_constraints" >::: [
    test "nothing added"              ~outer:[ "'n <= 8" ]            ~inner:[ "'n <= 8" ]                         ~expected:[];
    test "one added"                  ~outer:[ "'n <= 8" ]            ~inner:[ "'n <= 8"; "'m == 5" ]              ~expected:[ "'m == 5" ];
    test "added to an empty outer"    ~outer:[]                       ~inner:[ "'n == 32" ]                        ~expected:[ "'n == 32" ];
    test "repeated outer not added"   ~outer:[ "'n <= 8" ]            ~inner:[ "'n <= 8"; "'n <= 8"; "'m >= 1" ]   ~expected:[ "'m >= 1" ];
    test "without size variables"     ~outer:[]                       ~inner:[ "32 >= 64" ]                        ~expected:[ "32 >= 64" ];
  ]


let test_affected_size_variables =
  let test (name : string) ~(outer : string list) ~(inner : string list) ~(expected : string list) =
    name >:: fun _ ->
      let actual = BranchConstraints.affected_size_variables ~outer:(List.map ~f:parse_constraint outer) ~inner:(List.map ~f:parse_constraint inner)
      in
      assert_equal ~printer:Fn.id (kids_to_string @@ KidSet.of_list @@ List.map ~f:kid expected) (kids_to_string actual)
  in
  "affected_size_variables" >::: [
    test "nothing added"
      ~outer:[ "'n == 'm"; "'k <= 8" ]
      ~inner:[ "'n == 'm"; "'k <= 8" ]
      ~expected:[];
    test "directly constrained"
      ~outer:[ "'k <= 8" ]
      ~inner:[ "'k <= 8"; "'n == 32" ]
      ~expected:[ "n" ];
    test "linked through an outer constraint"
      ~outer:[ "'n == 'm"; "'k <= 8" ]
      ~inner:[ "'n == 'm"; "'k <= 8"; "'m == 5" ]
      ~expected:[ "m"; "n" ];
    test "linked through a chain"
      ~outer:[ "'a == 'b + 1"; "'b == 'c"; "'d == 7" ]
      ~inner:[ "'a == 'b + 1"; "'b == 'c"; "'d == 7"; "'c == 2" ]
      ~expected:[ "a"; "b"; "c" ];
    test "linked by a constraint over several variables"
      ~outer:[ "'a <= 'b"; "'c >= 0" ]
      ~inner:[ "'a <= 'b"; "'c >= 0"; "'b + 'c == 8" ]
      ~expected:[ "a"; "b"; "c" ];
    test "added without size variables"
      ~outer:[ "'n <= 8" ]
      ~inner:[ "'n <= 8"; "32 >= 64" ]
      ~expected:[];
  ]


(*
   Tests against Sail's solver: the filter is only correct if prove and solve_unique give the same answers
   inside the branch as outside for everything it skips. These tests break if a new version of Sail makes
   them depend on more than the environment's constraints.
*)
let environment (kids : string list) (constraints : string list) : TypeCheck.Env.t =
  let with_kids =
    List.fold kids ~init:TypeCheck.initial_env ~f:(fun environment name ->
        TypeCheck.Env.add_typ_var Libsail.Parse_ast.Unknown (U.mk_kopt S.K_int (kid name)) environment)
  in
  List.fold constraints ~init:with_kids ~f:(fun environment source ->
      TypeCheck.Env.add_constraint (parse_constraint source) environment)

let solve (environment : TypeCheck.Env.t) (name : string) : string =
  match TypeCheck.solve_unique environment (U.nvar (kid name)) with
  | Some value -> Z.to_string value
  | None       -> "not unique"

let proves_false (environment : TypeCheck.Env.t) : bool =
  TypeCheck.prove Stdlib.__POS__ environment U.nc_false


type expectation = {
  fixed_inside : (string * string) list;   (* affected size variables with the value the branch gives them *)
  dead         : bool;                     (* whether the branch is contradictory while its outside is not *)
}

let test_against_solver =
  let test
      (name     : string     )
      ~(kids    : string list)
      ~(outer   : string list)
      ~(added   : string list)
      (expected : expectation) =
    (* Sail starts the solver through an effect, which its main handles with toplevel_handler *)
    name >:: fun _ -> Libsail.Parmap.toplevel_handler @@ fun () ->
      let outer_environment = environment kids outer
      in
      let inner_environment = List.fold added ~init:outer_environment ~f:(fun environment source ->
          TypeCheck.Env.add_constraint (parse_constraint source) environment)
      in
      let outer_constraints = TypeCheck.Env.get_constraints outer_environment
      and inner_constraints = TypeCheck.Env.get_constraints inner_environment
      in
      let added_constraints = BranchConstraints.added_constraints ~outer:outer_constraints ~inner:inner_constraints
      and affected          = BranchConstraints.affected_size_variables ~outer:outer_constraints ~inner:inner_constraints
      in
      (* skipping the dead-branch check is correct *)
      if
        List.is_empty added_constraints
      then
        assert_equal ~msg:"no constraints added, yet the solver's verdict on false changed" ~printer:Bool.to_string
          (proves_false outer_environment) (proves_false inner_environment);
      assert_equal ~msg:"dead branch" ~printer:Bool.to_string
        expected.dead (proves_false inner_environment && not (proves_false outer_environment));
      if
        not expected.dead
      then begin
        (* skipping a size variable is correct: the solver gives the same answer inside and outside *)
        List.iter kids ~f:(fun name ->
            if
              not (KidSet.mem (kid name) affected)
            then
              assert_equal ~msg:(Printf.sprintf "'%s is not affected, yet the solver's answer changed" name) ~printer:Fn.id
                (solve outer_environment name) (solve inner_environment name));
        (* the variables the branch fixes are among the affected ones, with the expected values *)
        List.iter expected.fixed_inside ~f:(fun (name, value) ->
            assert_bool (Printf.sprintf "'%s should be affected (affected: %s)" name (kids_to_string affected)) (KidSet.mem (kid name) affected);
            assert_equal ~msg:(Printf.sprintf "'%s outside the branch" name) ~printer:Fn.id "not unique" (solve outer_environment name);
            assert_equal ~msg:(Printf.sprintf "'%s inside the branch" name) ~printer:Fn.id value (solve inner_environment name))
      end
  in
  "against Sail's solver" >::: [
    test "nothing added"
      ~kids:[ "n"; "m" ] ~outer:[ "'n <= 8"; "'m == 3" ] ~added:[]
      { fixed_inside = []; dead = false };
    test "repeated outer constraint"
      ~kids:[ "n" ] ~outer:[ "'n >= 0"; "'n <= 8" ] ~added:[ "'n <= 8" ]
      { fixed_inside = []; dead = false };
    test "directly fixed, others unchanged"
      ~kids:[ "n"; "m"; "k" ] ~outer:[ "'m == 4"; "'k >= 0" ] ~added:[ "'n == 32" ]
      { fixed_inside = [ ("n", "32") ]; dead = false };
    test "fixed through an outer equality"
      ~kids:[ "n"; "m"; "k" ] ~outer:[ "'n == 'm"; "'k <= 8" ] ~added:[ "'m == 5" ]
      { fixed_inside = [ ("n", "5"); ("m", "5") ]; dead = false };
    test "fixed through a chain"
      ~kids:[ "a"; "b"; "c"; "d" ] ~outer:[ "'a == 'b + 1"; "'b == 'c"; "'d == 7" ] ~added:[ "'c == 2" ]
      { fixed_inside = [ ("a", "3"); ("b", "2"); ("c", "2") ]; dead = false };
    test "bounds without fixing"
      ~kids:[ "n"; "m" ] ~outer:[ "'n >= 0"; "'m >= 0"; "'m <= 1" ] ~added:[ "'n <= 8" ]
      { fixed_inside = []; dead = false };
    test "fixed by bounds"
      ~kids:[ "n"; "m" ] ~outer:[ "'n >= 4"; "'m == 1" ] ~added:[ "'n <= 4" ]
      { fixed_inside = [ ("n", "4") ]; dead = false };
    test "contradictory branch"
      ~kids:[ "n" ] ~outer:[ "'n <= 8" ] ~added:[ "'n >= 16" ]
      { fixed_inside = []; dead = true };
    test "contradictory without size variables"
      ~kids:[ "n" ] ~outer:[ "'n <= 8" ] ~added:[ "32 >= 64" ]
      { fixed_inside = []; dead = true };
  ]


let test_suite =
  "branch constraints" >::: [
    test_added_constraints;
    test_affected_size_variables;
    test_against_solver;
  ]
