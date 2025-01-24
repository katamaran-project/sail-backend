open Base
open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext


let define_enum
    (identifier : Ast.Identifier.t     )
    (cases      : Ast.Identifier.t list) : unit TC.t
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
  TC.store_definition definition


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


let test_build_chain_enum =
  let test _ =
    let tc =
      TC.return ()
    in
    let result = run_tc tc
    in
    assert_equal () result
  in
  "building chain for (enum)" >:: test


let test_chain_building_suite =
  "chain building test suite" >::: [
  ]


let test_suite =
  "tuple pattern matching test suite" >::: [
    test_chain_building_suite
  ]
