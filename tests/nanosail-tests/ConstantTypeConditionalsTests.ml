open Base
open OUnit2

module S = Libsail.Ast
module U = Libsail.Ast_util
module Env = Libsail.Type_check.Env
module Fold = Nanosail.SailRewrites.ConstantTypeConditionals


let integer (value : int) : S.nexp =
  U.nconstant (Z.of_int value)

(* A type variable, about which nothing is known *)
let n : S.nexp =
  U.nvar (U.mk_kid "n")

(* A type synonym, type xlen : Int = 32 in env_with_xlen *)
let xlen : S.nexp =
  U.nid (U.mk_id "xlen")

let empty_env : Env.t =
  Libsail.Type_check.initial_env

let env_with_xlen : Env.t =
  Env.add_typ_synonym (U.mk_id "xlen") [] (S.A_aux (A_nexp (integer 32), Libsail.Parse_ast.Unknown)) empty_env

(* if 32 == 32 then 34 else 64 *)
let holds : S.nexp =
  U.nite (U.nc_eq (integer 32) (integer 32)) (integer 34) (integer 64)


let test_decide =
  let test (name : string) (env : Env.t) (condition : S.n_constraint) (expected : bool option) =
    name >:: fun _ -> begin
        let actual = Fold.decide env condition
        in
        let to_string (decision : bool option) : string =
          match decision with
          | Some true  -> "Some true"
          | Some false -> "Some false"
          | None       -> "None"
        in
        assert_equal ~printer:to_string ~cmp:(Option.equal Bool.equal) expected actual
      end
  in
  "decide" >::: [
    test "true"                         empty_env     U.nc_true                                              (Some true);
    test "false"                        empty_env     U.nc_false                                             (Some false);
    test "equal constants"              empty_env     (U.nc_eq (integer 32) (integer 32))                    (Some true);
    test "different constants"          empty_env     (U.nc_eq (integer 32) (integer 64))                    (Some false);
    test "comparison of constants"      empty_env     (U.nc_gt (integer 4) (integer 8))                      (Some false);
    test "constant in a set"            empty_env     (U.nc_set (integer 4) (List.map ~f:Z.of_int [1; 2; 4]))  (Some true);
    test "conjunction"                  empty_env     (U.nc_and U.nc_true (U.nc_eq (integer 1) (integer 2))) (Some false);
    test "synonym, expanded"            env_with_xlen (U.nc_eq xlen (integer 32))                            (Some true);
    test "synonym the env lacks"        empty_env     (U.nc_eq xlen (integer 32))                            None;
    test "type variable"                empty_env     (U.nc_eq n (integer 32))                               None;
  ]


let test_fold_numeric_expression =
  let test (name : string) (env : Env.t) (input : S.nexp) (expected : S.nexp) =
    name >:: fun _ -> begin
        let actual = Fold.fold_numeric_expression env input
        in
        assert_equal ~printer:Fn.id ~cmp:String.equal (U.string_of_nexp expected) (U.string_of_nexp actual)
      end
  in
  "fold_numeric_expression" >::: [
    test "condition that holds"         empty_env     holds                                                             (integer 34);
    test "condition that fails"         empty_env     (U.nite (U.nc_eq (integer 32) (integer 64)) (integer 34) (integer 64)) (integer 64);
    test "false"                        empty_env     (U.nite U.nc_false (integer 4) (integer 5))                       (integer 5);
    test "synonym in the condition"     env_with_xlen (U.nite (U.nc_eq xlen (integer 32)) (integer 34) (integer 64))    (integer 34);
    test "synonym the env lacks"        empty_env     (U.nite (U.nc_eq xlen (integer 32)) (integer 34) (integer 64))    (U.nite (U.nc_eq xlen (integer 32)) (integer 34) (integer 64));
    test "type variable"                empty_env     (U.nite (U.nc_eq n (integer 32)) (integer 34) (integer 64))       (U.nite (U.nc_eq n (integer 32)) (integer 34) (integer 64));
    (* only the conditional is folded, not the arithmetic around it *)
    test "inside arithmetic"            empty_env     (U.ntimes (integer 8) holds)                                      (U.ntimes (integer 8) (integer 34));
    test "in the selected branch"       empty_env     (U.nite U.nc_true holds (integer 0))                              (integer 34);
    test "in the condition"             empty_env     (U.nite (U.nc_eq holds (integer 34)) (integer 1) (integer 2))     (integer 1);
    (* the outer condition is unknown, the inner ones are folded *)
    test "in the branches of a conditional that stays"
                                        empty_env     (U.nite (U.nc_eq n (integer 1)) holds (U.nite U.nc_false (integer 1) (integer 2)))
                                                      (U.nite (U.nc_eq n (integer 1)) (integer 34) (integer 2));
    test "without conditional"          empty_env     (U.nsum n (integer 1))                                            (U.nsum n (integer 1));
  ]


let test_fold_type =
  let test (name : string) (input : S.typ) (expected : S.typ) =
    name >:: fun _ -> begin
        let actual = Fold.fold_type empty_env input
        in
        assert_equal ~printer:Fn.id ~cmp:String.equal (U.string_of_typ expected) (U.string_of_typ actual)
      end
  in
  let kinded_n = U.mk_kopt K_int (U.mk_kid "n")
  in
  "fold_type" >::: [
    test "bitvector"                    (U.bitvector_typ holds)                                (U.bitvector_typ (integer 34));
    test "tuple"                        (U.tuple_typ [U.bitvector_typ holds; U.atom_typ holds]) (U.tuple_typ [U.bitvector_typ (integer 34); U.atom_typ (integer 34)]);
    test "function"                     (U.function_typ [U.bitvector_typ holds] U.bool_typ)     (U.function_typ [U.bitvector_typ (integer 34)] U.bool_typ);
    test "constraint of an existential" (U.mk_typ (Typ_exist ([kinded_n], U.nc_eq n holds, U.atom_typ n)))
                                        (U.mk_typ (Typ_exist ([kinded_n], U.nc_eq n (integer 34), U.atom_typ n)));
    test "without conditional"          (U.bitvector_typ n)                                     (U.bitvector_typ n);
  ]


let test_suite =
  "constant type conditionals" >::: [
    test_decide;
    test_fold_numeric_expression;
    test_fold_type;
  ]
