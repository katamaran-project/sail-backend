open Base
open OUnit2

module S = Libsail.Ast
module U = Libsail.Ast_util
module TypeCheck = Libsail.Type_check
module ParameterPatterns = Nanosail.SailToNanosail.Translate.ParameterPatterns


(* A pattern of the given type; demote_parameters reads the type of each parameter it replaces *)
let pattern (unwrapped_pattern : TypeCheck.tannot S.pat_aux) (typ : S.typ) : TypeCheck.tannot S.pat =
  S.P_aux (unwrapped_pattern, (Libsail.Parse_ast.Unknown, TypeCheck.mk_tannot TypeCheck.initial_env typ))

let regidx : S.typ =
  U.mk_id_typ (U.mk_id "regidx")

let bits32 : S.typ =
  U.bitvector_typ (U.nconstant (Z.of_int 32))

let variable (name : string) (typ : S.typ) : TypeCheck.tannot S.pat =
  pattern (P_id (U.mk_id name)) typ

(* Regidx(i) *)
let constructor_pattern : TypeCheck.tannot S.pat =
  pattern (P_app (U.mk_id "Regidx", [ variable "i" (U.bitvector_typ (U.nconstant (Z.of_int 5))) ])) regidx

let tuple (elements : TypeCheck.tannot S.pat list) : TypeCheck.tannot S.pat =
  pattern (P_tuple elements) (U.tuple_typ (List.map elements ~f:TypeCheck.typ_of_pat))


let test_is_plain_parameter =
  let test (name : string) (input : TypeCheck.tannot S.pat) (expected : bool) =
    name >:: fun _ -> begin
        assert_equal ~printer:Bool.to_string expected (ParameterPatterns.is_plain_parameter input)
      end
  in
  "is_plain_parameter" >::: [
    test "name"                   (variable "x" bits32)                                                             true;
    test "wildcard"               (pattern P_wild bits32)                                                           true;
    test "unit"                   (pattern (P_lit (U.mk_lit L_unit)) U.unit_typ)                                    true;
    test "name with a type"       (pattern (P_typ (bits32, variable "x" bits32)) bits32)                            true;
    test "constructor"            constructor_pattern                                                               false;
    test "constructor with a type" (pattern (P_typ (regidx, constructor_pattern)) regidx)                           false;
    test "tuple"                  (tuple [ variable "a" bits32; variable "b" bits32 ])                              false;
    test "literal"                (pattern (P_lit (U.mk_lit L_true)) U.bool_typ)                                    false;
  ]


let test_demote_parameters =
  let test
      (name               : string                 )
      (input              : TypeCheck.tannot S.pat )
      (expected_pattern   : string                 )
      (expected_demoted   : (string * string) list )
    =
    name >:: fun _ -> begin
        let actual_pattern, actual_demoted = ParameterPatterns.demote_parameters input
        in
        let describe (demoted : ParameterPatterns.demoted_parameter) : string * string =
          (U.string_of_id demoted.identifier, U.string_of_pat demoted.pattern)
        in
        assert_equal ~printer:Fn.id ~cmp:String.equal expected_pattern (U.string_of_pat actual_pattern);
        assert_equal
          ~printer:(fun pairs -> String.concat ~sep:"; " (List.map pairs ~f:(fun (identifier, pattern) -> identifier ^ " = " ^ pattern)))
          ~cmp:(List.equal (fun (i1, p1) (i2, p2) -> String.equal i1 i2 && String.equal p1 p2))
          expected_demoted
          (List.map ~f:describe actual_demoted)
      end
  in
  "demote_parameters" >::: [
    test "plain name stays"
      (variable "x" bits32)
      "x"
      [];
    test "single constructor parameter"
      constructor_pattern
      "arg#0"
      [ ("arg#0", "Regidx(i)") ];
    test "constructor parameter with a type"
      (pattern (P_typ (regidx, constructor_pattern)) regidx)
      "arg#0"
      [ ("arg#0", "Regidx(i) : regidx") ];
    test "plain parameters stay"
      (tuple [ variable "a" bits32; variable "b" bits32 ])
      "(a, b)"
      [];
    test "only the patterns among several parameters"
      (tuple [ constructor_pattern; variable "data" bits32; constructor_pattern ])
      "(arg#0, data, arg#1)"
      [ ("arg#0", "Regidx(i)"); ("arg#1", "Regidx(i)") ];
    test "nested tuple"
      (tuple [ tuple [ variable "a" bits32; variable "b" bits32 ]; variable "c" bits32 ])
      "(arg#0, c)"
      [ ("arg#0", "(a, b)") ];
  ]


let test_demoted_parameter_type =
  "demoted parameter keeps the type of its pattern" >:: fun _ -> begin
      match ParameterPatterns.demote_parameters (tuple [ constructor_pattern; variable "data" bits32 ]) with
      | (_, [ demoted ]) -> assert_equal ~printer:U.string_of_typ ~cmp:(fun t1 t2 -> String.equal (U.string_of_typ t1) (U.string_of_typ t2)) regidx demoted.typ
      | (_, demoted)     -> assert_failure (Printf.sprintf "expected one demoted parameter, got %d" (List.length demoted))
    end


let test_suite =
  "parameter patterns" >::: [
    test_is_plain_parameter;
    test_demote_parameters;
    test_demoted_parameter_type;
  ]
