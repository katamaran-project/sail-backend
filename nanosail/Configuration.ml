open! ExtBase

type sail_definition = Sail.sail_definition


module C = struct
  include ConfigLib.BuildContext.Make(struct end)

  module EC = Slang.EvaluationContext
  module C  = Slang.Converters

  open Slang.Prelude.Shared

  module S = struct
    let output_width                         = integer  "output-width"                     200
    let use_list_notations                   = bool     "use-list-notations"               false (* Use list notations                                                          *)
    let include_original_code                = bool     "include-original-code"            true  (* Annotate all Microsail definitions with their corresponding Sail definition *)
    let ignore_overloads                     = bool     "ignore-all-overloads"             false (* Ignore all overloads                                                        *)
    let ignore_default_order                 = bool     "ignore-default-order"             true
    let print_warnings                       = bool     "print-warnings"                   false
    let pretty_print_let                     = bool     "pretty-print-let"                 false
    let pretty_print_match_enum              = bool     "pretty-print-match-enum"          false
    let pretty_print_binary_operators        = bool     "pretty-print-binary-operators"    false
    let show_generation_blocks               = bool     "show-generation-blocks"           false
    let bitvectors_zeros_ones_as_literal     = bool     "literal-zeros-and-ones"           false (* Use bv 0 instead of Bitvector.bv.zero (same for ones)                       *)
    let inline_definitions_in_notations      = bool     "inline-definitions-in-notations"  true
    let annotate_functions_with_ast          = bool     "annotate-functions-with-ast"      false
    let ignored_pragmas                      = strings  "ignore-pragmas"                         (* Pragmas to be ignored                                                       *)
    let ignored_functions                    = strings  "ignore-functions"                       (* Functions to be ignored                                                     *)
    let base_name                            = string   "base-name"                        "UntitledBase"
    let program_name                         = string   "program-name"                     "ModelProgram"
    let ignore_type_definition_predicate     = callable ~error_message:"missing ignore-type-definition-predicate" "ignore-type-definition-predicate"
    let ignore_value_definition_predicate    = callable ~error_message:"missing ignore-value-definition-predicate" "ignore-value-definition-predicate"
    let ignore_function_definition_predicate = callable' "ignore-function-definition-predicate" (constant_function ~arity:1 ~return_value:(Slang.Value.Bool false))


    (* template block delimiters *)
    let template_block_left_delimiter     = string "template-block-left-delimiter"  "(*<"
    let template_block_right_delimiter    = string "template-block-right-delimiter" ">*)"

    (* helper function template-block-delimiters that allows setting both template block delimiters in one step *)
    let () = export_strict_function "template-block-delimiters" @@ fun evaluated_arguments -> begin
        let=! left, right = C.(map2 string string) evaluated_arguments
        in
        ConfigLib.Setting.set template_block_left_delimiter left;
        ConfigLib.Setting.set template_block_right_delimiter right
      end


    (*
       Template translations

       It is possible to specify multiple templates to be translated.
     *)
    type template_translation =
      {
        template_filename : string;
        output_filename   : string;
      }

    let template_translations = ConfigLib.Setting.mk ([] : template_translation list)

    let () =
      let exported_function_name = "template"
      and add_translation template_filename output_filename =
        ConfigLib.Setting.update template_translations (fun x -> { template_filename; output_filename } :: x)
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
          match C.(map1 C.string) evaluated_arguments with
          | Some template_filename -> begin
              let output_filename = derive_output_filename_from_template_filename template_filename
              in
              add_translation template_filename output_filename;
              EC.return @@ Some Slang.Value.Nil
            end
          | None -> EC.return None
        and binary_version evaluated_arguments =
          match C.(map2 C.string C.string) evaluated_arguments with
          | Some (template_filename, output_filename) -> begin
              add_translation template_filename output_filename;
              EC.return @@ Some Slang.Value.Nil
            end
          | None -> EC.return None
        in
        Slang.Functions.mk_multimethod [ unary_version; binary_version ]
      in
      export_callable exported_function_name handler_function
  end
end

include C.S

let load_configuration = C.load_configuration
let get                = ConfigLib.Setting.get
let set                = ConfigLib.Setting.set


let should_ignore_definition (definition : Sail.sail_definition) : bool =
  let open Libsail.Ast
  in
  let Libsail.Ast.DEF_aux (definition, _annotation) = definition
  in
  let member setting item =
    List.mem (get setting) item ~equal:String.equal
  in

  let should_ignore_pragma identifier =
    member ignored_pragmas identifier

  and should_ignore_function_definition function_definition =
    let identifier = Sail.identifier_of_function_definition function_definition
    in
    let arguments = [ Slang.Value.String identifier ]
    in
    let result, _ = Slang.EvaluationContext.run @@ get ignore_function_definition_predicate arguments
    in
    match result with
    | C.EC.Success result -> Slang.Value.truthy result
    | C.EC.Failure _      -> failwith "Error while reading configuration"

  and should_ignore_type_definition type_definition =
    let identifier = Sail.identifier_of_type_definition type_definition
    in
    let arguments = [ Slang.Value.String identifier ]
    in
    let result, _ = Slang.EvaluationContext.run @@ get ignore_type_definition_predicate arguments
    in
    match result with
    | C.EC.Success result -> Slang.Value.truthy result
    | C.EC.Failure _      -> failwith "Error while reading configuration"

  and should_ignore_value_definition (value_definition : Libsail.Type_check.tannot letbind) =
    let LB_aux (LB_val (pattern, E_aux (_, _)), (_location2, _type_annotation)) = value_definition
    in
    let identifier = Sail.identifier_of_pattern pattern
    in
    let arguments  = [ Slang.Value.String identifier ]
    in
    let result, _  = Slang.EvaluationContext.run @@ get ignore_value_definition_predicate arguments
    in
    match result with
    | C.EC.Success result -> Slang.Value.truthy result
    | C.EC.Failure _      -> failwith "Error while reading configuration"

  and should_ignore_default_definition (_default_spec : default_spec) : bool =
    get ignore_default_order

  in
  match definition with
  | DEF_type type_definition       -> should_ignore_type_definition type_definition
  | DEF_fundef function_definition -> should_ignore_function_definition function_definition
  | DEF_mapdef _                   -> false
  | DEF_impl _                     -> false
  | DEF_let value_definition       -> should_ignore_value_definition value_definition
  | DEF_val _                      -> false
  | DEF_outcome (_, _)             -> false
  | DEF_instantiation (_, _)       -> false
  | DEF_fixity (_, _, _)           -> false
  | DEF_overload (_, _)            -> get (ignore_overloads)
  | DEF_default default_spec       -> should_ignore_default_definition default_spec
  | DEF_scattered _                -> false
  | DEF_measure (_, _, _)          -> false
  | DEF_loop_measures (_, _)       -> false
  | DEF_register _                 -> false
  | DEF_internal_mutrec _          -> false
  | DEF_constraint _               -> false
  | DEF_pragma (identifier, _, _)  -> should_ignore_pragma identifier
