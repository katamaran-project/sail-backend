open Base

include ConfigLib.Exported


type sail_definition = Ast.sail_definition

let use_list_notations               = ConfigLib.bool     "use-list-notations"               (* Use list notations                                                          *)
let include_untranslated_definitions = ConfigLib.bool     "include-untranslated-definitions" (* Output definitions for which no translation is available yet                *)
let include_original_code            = ConfigLib.bool     "include-original-code"            (* Annotate all Microsail definitions with their corresponding Sail definition *)
let include_ignored_definitions      = ConfigLib.bool     "include-ignored-definitions"      (* Output ignored definitions                                                  *)
let ignored_pragmas                  = ConfigLib.strings  "ignore-pragmas"                   (* Pragmas to be ignored                                                       *)
let ignored_functions                = ConfigLib.strings  "ignore-functions"                 (* Functions to be ignored                                                     *)
let ignore_overloads                 = ConfigLib.bool     "ignore-all-overloads"             (* Ignore all overloads                                                        *)
let ignore_definition_predicate      = ConfigLib.callable ~error_message:"missing ignore-definition-predicate" "ignore-definition-predicate"


module Identifier = struct
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
    | _ -> failwith "wanted to extract funtion name from function definition; failed because I didn't recognize structure"

  let of_type_definition (TD_aux (definition, (_location, _))) =
    match definition with
    | TD_abbrev (id, _, _)     -> string_of_id id
    | TD_record (id, _, _, _)  -> string_of_id id
    | TD_variant (id, _, _, _) -> string_of_id id
    | TD_enum (id, _, _)       -> string_of_id id
    | TD_bitfield (id, _, _)   -> string_of_id id
end


let ignore_definition (Libsail.Ast.DEF_aux (definition, _annotation)) =
  let open Libsail.Ast
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
    let result, _ = Slang.EvaluationContext.run (get ignore_definition_predicate arguments) Slang.Environment.empty
    in
    Slang.Value.truthy result

  in
  match definition with
  | DEF_pragma (identifier, _, _)  -> should_ignore_pragma identifier
  | DEF_fundef function_definition -> should_ignore_function_definition function_definition
  | DEF_type type_definition       -> should_ignore_type_definition type_definition
  | DEF_overload (_, _)            -> get (ignore_overloads)
  | _                              -> false
