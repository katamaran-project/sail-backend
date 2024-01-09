open Base

include ConfigLib.Exported


type sail_definition = Ast.sail_definition

let use_list_notations               = ConfigLib.bool    "use-list-notations"               (* Use list notations                                                          *)
let include_untranslated_definitions = ConfigLib.bool    "include-untranslated-definitions" (* Output definitions for which no translation is available yet                *)
let include_original_code            = ConfigLib.bool    "include-original-code"            (* Annotate all Microsail definitions with their corresponding Sail definition *)
let include_ignored_definitions      = ConfigLib.bool    "include-ignored-definitions"      (* Output ignored definitions                                                  *)
let ignore_pragmas                   = ConfigLib.strings "ignore-pragmas"                   (* Pragmas to be ignored                                                       *)
let ignore_functions                 = ConfigLib.strings "ignore-functions"                 (* Functions to be ignored                                                     *)
let ignore_overloads                 = ConfigLib.bool    "ignore-all-overloads"             (* Ignore all overloads                                                        *)
let ignore_types                     = ConfigLib.strings "ignore-types"                     (* Types to be ignored                                                         *)

let ignore_definition (Libsail.Ast.DEF_aux (definition, _annotation)) =
  match definition with
  | Libsail.Ast.DEF_pragma (identifier, _, _) ->
     List.mem (get ignore_pragmas) identifier ~equal:String.equal
  | Libsail.Ast.DEF_fundef function_definition ->
     let identifier = Sail.identifier_of_function_definition function_definition
     in
     List.mem (get ignore_functions) identifier ~equal:String.equal
  | Libsail.Ast.DEF_overload (_, _) -> get (ignore_overloads)
  | Libsail.Ast.DEF_type type_definition ->
     let identifier = Sail.identifier_of_type_definition type_definition
     in
     List.mem (get ignore_types) identifier ~equal:String.equal
  | _ -> false
