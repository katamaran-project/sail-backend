open Base


type sail_definition = Ast.sail_definition


module C = struct
  include ConfigLib.BuildContext.M(struct end)

  module EC = Slang.EvaluationContext
  module C  = Slang.Converters

  open Slang.Prelude.Shared

  module S = struct
    let output_width                      = integer  "output-width"                     80
    let use_list_notations                = bool     "use-list-notations"               false (* Use list notations                                                          *)
    let include_original_code             = bool     "include-original-code"            false (* Annotate all Microsail definitions with their corresponding Sail definition *)
    let ignore_overloads                  = bool     "ignore-all-overloads"             false (* Ignore all overloads                                                        *)
    let print_warnings                    = bool      "print-warnings"                  false
    let ignored_pragmas                   = strings  "ignore-pragmas"                         (* Pragmas to be ignored                                                       *)
    let ignored_functions                 = strings  "ignore-functions"                       (* Functions to be ignored                                                     *)
    let ignore_definition_predicate       = callable ~error_message:"missing ignore-definition-predicate"       "ignore-definition-predicate"
    let ignore_value_definition_predicate = callable ~error_message:"missing ignore-value-definition-predicate" "ignore-value-definition-predicate"
    let template_files                    = string_to_string "template"

    let template_block_left_delimiter     = ConfigLib.Setting.mk "(*<"
    let template_block_right_delimiter    = ConfigLib.Setting.mk ">*)"

    let _ = export_strict_function "template-block-delimiters" @@ fun evaluated_arguments -> begin
          let=! left, right = C.(map2 C.string C.string) evaluated_arguments
          in
          ConfigLib.Setting.set template_block_left_delimiter left;
          ConfigLib.Setting.set template_block_right_delimiter right
        end
  end
end

include C.S

let load_configuration = C.load_configuration
let get                = ConfigLib.Setting.get
let set                = ConfigLib.Setting.set

module Identifier = struct (* move somewhere else *)
  open Libsail.Ast

  (* extracts name as string; fails on operator name *) (* todo function already exists somewhere else *)
  let string_of_id (id : id) : string =
    let Id_aux (id, _loc) = id
    in
    match id with
    | Id s       -> s
    | Operator _ -> failwith "operator names not supported"

  (* determines the name of a function *)
  let of_function_definition (function_definition : 'a fundef) =
    let FD_aux (FD_function (_, _, x), (_location, _)) = function_definition
    in
    match x with
    | [ FCL_aux (Libsail.Ast.FCL_funcl (Libsail.Ast.Id_aux (Id identifier, _), _), _) ] -> identifier
    | _ -> failwith "wanted to extract function name from function definition; failed because I didn't recognize structure"

  let of_type_definition (TD_aux (definition, (_location, _))) =
    match definition with
    | TD_abbrev (id, _, _)     -> string_of_id id
    | TD_record (id, _, _, _)  -> string_of_id id
    | TD_variant (id, _, _, _) -> string_of_id id
    | TD_enum (id, _, _)       -> string_of_id id
    | TD_bitfield (id, _, _)   -> string_of_id id

  let rec of_pattern (pattern : 'a pat) : string =
    let P_aux (pattern, _) = pattern
    in
    match pattern with
     | P_id identifier    -> string_of_id identifier
     | P_typ (_, pattern) -> of_pattern pattern
     | P_lit _ -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_wild -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_or (_, _) -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_not _ -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_as (_, _) -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_var (_, _) -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_app (_, _) -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_vector _ -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_vector_concat _ -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_vector_subrange (_, _, _) -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_tuple _ -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_list _ -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_cons (_, _) -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_string_append _ -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
     | P_struct (_, _) -> begin
         let error_message = Printf.sprintf "not supported (%s)" @@ Basics.string_of_position [%here]
         in
         failwith error_message
       end
end


let should_ignore_definition (definition : Libsail.Type_check.tannot Libsail.Ast.def) : bool =
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
    member ignored_functions @@ Identifier.of_function_definition function_definition

  and should_ignore_type_definition type_definition =
    let identifier = Identifier.of_type_definition type_definition
    in
    let arguments = [ Slang.Value.String identifier ]
    in
    let result, _ = Slang.EvaluationContext.run @@ get ignore_definition_predicate arguments
    in
    Slang.Value.truthy result

  and should_ignore_value_definition (value_definition : Libsail.Type_check.tannot letbind) =
    let LB_aux (LB_val (pattern, E_aux (_, _)), (_location2, _type_annotation)) = value_definition
    in
    let identifier = Identifier.of_pattern pattern
    in
    let arguments  = [ Slang.Value.String identifier ]
    in
    let result, _  = Slang.EvaluationContext.run @@ get ignore_value_definition_predicate arguments
    in
    Slang.Value.truthy result

  in
  match definition with
  | DEF_pragma (identifier, _, _)  -> should_ignore_pragma identifier
  | DEF_fundef function_definition -> should_ignore_function_definition function_definition
  | DEF_type type_definition       -> should_ignore_type_definition type_definition
  | DEF_let value_definition       -> should_ignore_value_definition value_definition
  | DEF_overload (_, _)            -> get (ignore_overloads)
  | _                              -> false
