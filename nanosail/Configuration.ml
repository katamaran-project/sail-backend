open! ExtBase


type monomorphization_request =
  {
    monomorphization_identifier : Ast.Identifier.t;
    substitutions               : (Ast.Identifier.t * int) list;
  }

type template_translation =
  {
    template_filename : string;
    output_filename   : string;
  }


module Implementation = struct
  include ConfigLib.BuildContext.Make(struct end)

  module EC = Slang.EvaluationContext

  open Slang.Prelude.Shared

  module Exported = struct
    let pattern_tree_maximum_permutations          = integer  "pattern-tree-max-permutations"    1000
    let include_original_code                      = bool     "include-original-code"            true  (* Annotate all Microsail definitions with their corresponding Sail definition *)
    let ignore_overloads                           = bool     "ignore-all-overloads"             false (* Ignore all overloads                                                        *)
    let ignore_default_order                       = bool     "ignore-default-order"             true
    let use_list_notations                         = bool     "pretty-print-lists"               false (* Use list notations                                                          *)
    let pretty_print_let                           = bool     "pretty-print-let"                 false
    let pretty_print_match_enum                    = bool     "pretty-print-match-enum"          false
    let pretty_print_binary_operators              = bool     "pretty-print-binary-operators"    false
    let pretty_print_function_calls                = bool     "pretty-print-function-calls"      false
    let show_generation_blocks                     = bool     "show-generation-blocks"           false
    let bitvectors_zeros_ones_as_literal           = bool     "literal-zeros-and-ones"           false (* Use bv 0 instead of Bitvector.bv.zero (same for ones)                       *)
    let inline_definitions_in_notations            = bool     "inline-definitions-in-notations"  true
    let annotate_functions_with_ast                = bool     "annotate-functions-with-ast"      false
    let base_name                                  = string   "base-name"                        "UntitledBase"
    let program_name                               = string   "program-name"                     "ModelProgram"
    
    let ignore_pragma_predicate                    = string_predicate "ignore-pragmas"                             (Fn.const false)
    let ignore_type_definition_predicate           = string_predicate "ignore-type-definition-predicate"           (Fn.const false)
    let ignore_value_definition_predicate          = string_predicate "ignore-value-definition-predicate"          (Fn.const false)
    let ignore_function_definition_predicate       = string_predicate "ignore-function-definition-predicate"       (Fn.const false)
    let ignore_top_level_type_constraint_predicate = string_predicate "ignore-top-level-type-constraint-predicate" (Fn.const false)

    let template_block_left_delimiter              = string "template-block-left-delimiter"  "(*<"
    let template_block_right_delimiter             = string "template-block-right-delimiter" ">*)"

    let template_translations                      = ConfigLib.Setting.mk ([] : template_translation list)
  end

  (*
     Defines Slang function named template.

       (template path)
  *)
  let () =
    let exported_function_name = "template"
    and add_translation template_filename output_filename =
      ConfigLib.Setting.update Exported.template_translations ~f:(fun x -> { template_filename; output_filename } :: x)
    and derive_output_filename_from_template_filename (template_filename : string) : string =
      (*
         Given the filename of the template file, derives an output filename from it.
         This is achieved by looking for a ".template" substring in template_filename and remove it.
       *)
      let pattern = String.Search_pattern.create ".template"
      in
      match String.Search_pattern.index_all pattern ~may_overlap:false ~in_:template_filename with
      | [] -> begin
          let error_message = Printf.sprintf "Error: when using (template %s), %s must contain the substring \".template\"." template_filename template_filename
          in
          failwith error_message
        end
      | [_] -> begin
          String.Search_pattern.replace_first pattern ~in_:template_filename ~with_:""
        end
      | _  -> begin
          let error_message = Printf.sprintf "Error: when using (template %s), %s must contain \".template\" at most once" template_filename template_filename
          in
          failwith error_message
        end
    in
    let handler_function =
      let unary_version evaluated_arguments =
        match Slang.Converters.(map1 string) evaluated_arguments with
        | Some template_filename -> begin
            let output_filename = derive_output_filename_from_template_filename template_filename
            in
            add_translation template_filename output_filename;
            EC.return @@ Some Slang.Value.Nil
          end
        | None -> EC.return None
      and binary_version evaluated_arguments =
        match Slang.Converters.(map2 string string) evaluated_arguments with
        | Some (template_filename, output_filename) -> begin
            add_translation template_filename output_filename;
            EC.return @@ Some Slang.Value.Nil
          end
        | None -> EC.return None
      in
      Slang.Functions.mk_multimethod [ unary_version; binary_version ]
    in
    export_callable exported_function_name handler_function  

  (* helper function template-block-delimiters that allows setting both template block delimiters in one step *)
  let () = export_strict_function "template-block-delimiters" @@ fun evaluated_arguments -> begin
      let=! left, right = Slang.Converters.(map2 string string) evaluated_arguments
      in
      ConfigLib.Setting.set Exported.template_block_left_delimiter left;
      ConfigLib.Setting.set Exported.template_block_right_delimiter right
    end
 

  let () =
    let exported_function_name =
      "ignore-function-definition-and-top-level-type-constraints-predicate"
    and setter (predicate : string -> bool) : unit =
      (ConfigLib.Setting.set Exported.ignore_function_definition_predicate) predicate;
      (ConfigLib.Setting.set Exported.ignore_top_level_type_constraint_predicate) predicate
    in
    export_string_predicate_setter exported_function_name setter


  let monomorphization_requests : monomorphization_request list Ast.Identifier.Map.t ref = ref Ast.Identifier.Map.empty

  (*
     Adds an extra entry to monomorphization_requests.
  *)
  let register_monomorphization_request
      (polymorphic_identifier : Ast.Identifier.t        )
      (request                : monomorphization_request) : unit
    =
    let update (requests : monomorphization_request list option) : monomorphization_request list =
      let requests =
        Option.value ~default:[] requests
      in
      request :: requests
    in
    monomorphization_requests := Ast.Identifier.Map.update !monomorphization_requests polymorphic_identifier ~f:update


  let () =
    let exported_function_name =
      "monomorphize"
    and handler_function arguments =
      let open Slang in
      let open Slang.Prelude.Shared
      in
      let open Monads.Notations.Star(Slang.EvaluationContext)
      in
      let* evaluated_arguments = EC.map ~f:Evaluation.evaluate arguments
      in
      let=! polymorphic_identifier, monomorphization_identifier, substitutions =
        Converters.(map3 string string (list (tuple2 string integer))) evaluated_arguments
      in
      let polymorphic_identifier      = Ast.Identifier.mk polymorphic_identifier
      and monomorphization_identifier = Ast.Identifier.mk monomorphization_identifier
      and substitutions               = List.map ~f:(fun (id, n) -> (Ast.Identifier.mk id, n)) substitutions
      in
      let monomorphization_request : monomorphization_request = {
        monomorphization_identifier;
        substitutions;
      }
      in
      register_monomorphization_request polymorphic_identifier monomorphization_request;
      EC.return @@ Value.Nil
    in
    export_callable exported_function_name handler_function
end

include Implementation.Exported


let requested_monomorphizations_for (function_identifier : Ast.Identifier.t) : monomorphization_request list option =
  Ast.Identifier.Map.find !Implementation.monomorphization_requests function_identifier


let load_configuration = Implementation.load_configuration
let get                = ConfigLib.Setting.get
let set                = ConfigLib.Setting.set
