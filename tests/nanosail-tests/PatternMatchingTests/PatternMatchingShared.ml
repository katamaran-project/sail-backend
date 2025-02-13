open Base
open OUnit2
open Nanosail
include Shared

module Pattern = SailToNanosail.Translate.Match.Pattern
module M       = SailToNanosail.Translate.Match


let build_empty_pattern_tree = M.build_empty_pattern_tree dummy_location
let adorn                    = M.adorn_pattern_tree dummy_location
let build_match              = M.build_leveled_match_statements


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
    { identifier = Ast.Identifier.mk_generated @@ Int.to_string self#next; wildcard = true }
end


let assert_equal_pattern_trees
    (expected : M.PatternTree.t)
    (actual   : M.PatternTree.t) : unit
  =
  assert_equal
    ~cmp:M.PatternTree.equal
    ~pp_diff:(pp_diff M.PatternTree.to_fexpr)
    (Normalize.normalize_pattern_tree expected)
    (Normalize.normalize_pattern_tree actual)


module TC = struct
  include SailToNanosail.TranslationContext
  open Monads.Notations.Star(SailToNanosail.TranslationContext)

  let assert_equal_pattern_trees
      (expected : M.PatternTree.t)
      (actual   : M.PatternTree.t) : unit t
    =
    return @@ assert_equal_pattern_trees expected actual


  let assert_equal_statements
      (expected : Ast.Statement.t)
      (actual   : Ast.Statement.t) : unit t
    =
    return @@ assert_equal_statements expected actual


  let define_enum
      (identifier : Ast.Identifier.t     )
      (cases      : Ast.Identifier.t list) : Ast.Type.t t
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
    let* () = store_definition definition
    in
    return @@ Ast.Type.Enum identifier


  let define_enum_str
      (identifier : string     )
      (cases      : string list) : Ast.Type.t t
    =
    let identifier = Ast.Identifier.mk identifier
    and cases      = List.map ~f:Ast.Identifier.mk cases
    in
    define_enum identifier cases


  let define_variant
      (identifier   : string                         )
      (constructors : (string * Ast.Type.t list) list) : Ast.Type.t t
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
    let* () = store_definition definition
    in
    return @@ Ast.Type.Variant identifier

  
  let run_expecting_success (tc : 'a t) : 'a =
    let result, _ = run tc
    in
    match result with
    | Success result -> result
    | Failure error  -> begin
        let error_message =
          Printf.sprintf "execution of TC resulted in failure: %s" @@ Error.to_string error
        in
        assert_failure error_message
      end


  let run_expecting_failure (tc : 'a t) : unit =
    let result, _ = run tc
    in
    match result with
    | Success _ -> assert_failure "expected failure but succeeded instead"
    | Failure _ -> ()
end
