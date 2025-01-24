open Base
open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)

module TM = SailToNanosail.Translate.Match.TupleMatching


let dummy_location : Libsail.Ast.l =
  Libsail.Parse_ast.Unknown

let mkid = Ast.Identifier.mk

let define_enum
    (identifier : Ast.Identifier.t     )
    (cases      : Ast.Identifier.t list) : Ast.Type.t TC.t
  =
  let enum_definition : Ast.Definition.Type.Enum.t =
    {
      identifier;
      cases
    }
  in
  let definition =
    Ast.Definition.TypeDefinition (Ast.Definition.Type.Enum enum_definition)
  in
  let* () = TC.store_definition definition
  in
  TC.return @@ Ast.Type.Enum identifier


let define_enum_str
    (identifier : string     )
    (cases      : string list) : Ast.Type.t TC.t
  =
  let identifier = Ast.Identifier.mk identifier
  and cases      = List.map ~f:Ast.Identifier.mk cases
  in
  define_enum identifier cases


let run_tc (tc : 'a TC.t) : 'a =
  let result, _ = TC.run tc
  in
  match result with
  | TC.Success result -> result
  | TC.Failure error  -> begin
      let error_message =
        Printf.sprintf "execution of TC resulted in failure: %s" @@ TC.Error.to_string error
      in
      assert_failure error_message
    end


let build_tuple_pattern_chain = TM.build_tuple_pattern_chain dummy_location


let test_build_chain_enum_1 =
  let test _ =
    let tc =
      let* enum_type =
        define_enum_str "A" ["A1"; "A2"]
      in
      let* chain =
        build_tuple_pattern_chain [ enum_type ]
      in
      match chain with
      | Enum { enum_identifier; table } -> begin
          assert_equal ~cmp:Ast.Identifier.equal (mkid "A") enum_identifier;
          assert_equal ~cmp:Int.equal 2 (Ast.Identifier.Map.length table);
          let expected_table : TM.PatternNode.t Ast.Identifier.Map.t =
            Ast.Identifier.Map.of_alist_exn [
              (
                mkid "A1",
                TM.PatternNode.Terminal None
              );
              (
                mkid "A2",
                TM.PatternNode.Terminal None
              );
            ]
          in
          assert_equal ~cmp:(Ast.Identifier.Map.equal TM.PatternNode.equal) expected_table table;
          TC.return ()
        end
       | Terminal _ -> assert_failure "expected enum node"
    in
    ignore @@ run_tc tc

  in
  "building chain for (enum)" >:: test


let test_chain_building_suite =
  "chain building test suite" >::: [
    test_build_chain_enum_1;
  ]


let test_suite =
  "tuple pattern matching test suite" >::: [
    test_chain_building_suite
  ]
