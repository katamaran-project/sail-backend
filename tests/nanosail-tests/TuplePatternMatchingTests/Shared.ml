open Base
open OUnit2
open Nanosail

module TC = SailToNanosail.TranslationContext
open Monads.Notations.Star(TC)

module Pattern = SailToNanosail.Translate.Match.Pattern
module TM      = SailToNanosail.Translate.Match.TupleMatching


let dummy_location : Libsail.Ast.l =
  Libsail.Parse_ast.Unknown

let mkid                                                          = Ast.Identifier.mk
let mkbinder identifier : SailToNanosail.Translate.Match.Binder.t = { identifier = mkid identifier; wildcard = false }
let mkwild   n          : SailToNanosail.Translate.Match.Binder.t = { identifier = Ast.Identifier.mk_generated @@ Int.to_string n; wildcard = true }
let mkstm    n                                                    = Ast.Statement.ReadRegister (mkid @@ Printf.sprintf "r%d" n)


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


let define_variant
    (identifier   : string                         )
    (constructors : (string * Ast.Type.t list) list) : Ast.Type.t TC.t
  =
  let identifier      = Ast.Identifier.mk identifier
  and type_quantifier = Ast.TypeQuantifier.TypeQuantifier []
  and constructors    = List.map ~f:(fun (id, ts) -> (Ast.Identifier.mk id, ts)) constructors
  in
  let variant_definition : Ast.Definition.Type.Variant.t =
    {
      identifier;
      type_quantifier;
      constructors;
    }
  in
  let definition =
    Ast.Definition.TypeDefinition (Ast.Definition.Type.Variant variant_definition)
  in
  let* () = TC.store_definition definition
  in
  TC.return @@ Ast.Type.Variant identifier


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


let run_failing_tc (tc : 'a TC.t) : unit =
  let result, _ = TC.run tc
  in
  match result with
  | TC.Success _ -> assert_failure "expected failure but succeeded instead"
  | TC.Failure _ -> ()


let build_tuple_pattern_tree = TM.build_tuple_pattern_tree dummy_location
let categorize               = TM.categorize_case dummy_location
let build_match              = TM.build_leveled_match_statements


let create_identifier_generator () =
  let counter = ref 0
  in
  let next () =
    let number = !counter
    in
    counter := !counter + 1;
    Ast.Identifier.mk_generated @@ Int.to_string number
  in
  next


class generator = object(self)
  val mutable counter = 0

  method private next =
    let result = counter
    in
    counter <- result + 1;
    result

  method id =
    Ast.Identifier.mk_generated @@ Int.to_string self#next

  method binder : SailToNanosail.Translate.Match.Binder.t =
    mkbinder @@ Int.to_string self#next
    
  method wildcard : SailToNanosail.Translate.Match.Binder.t =
    mkwild self#next
end

module Normalize = Normalize
