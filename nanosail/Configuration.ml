open Base

include ConfigLib.Exported


type sail_definition = Ast.sail_definition

let use_list_notations               = ConfigLib.bool    "use-list-notations"               (* Use list notations                                                          *)
let include_untranslated_definitions = ConfigLib.bool    "include-untranslated-definitions" (* Output definitions for which no translation is available yet                *)
let include_original_code            = ConfigLib.bool    "include-original-code"            (* Annotate all Microsail definitions with their corresponding Sail definition *)
let include_ignored_definitions      = ConfigLib.bool    "include-ignored-definitions"      (* Output ignored definitions                                                  *)
let ignored_pragmas                  = ConfigLib.strings "ignore-pragmas"                   (* Pragmas to be ignored                                                       *)
let ignored_functions                = ConfigLib.strings "ignore-functions"                 (* Functions to be ignored                                                     *)
let ignore_overloads                 = ConfigLib.bool    "ignore-all-overloads"             (* Ignore all overloads                                                        *)
let ignored_types                    = ConfigLib.strings "ignore-types"                     (* Types to be ignored                                                         *)


module Identifier = struct
  open Libsail.Ast
  open NYI
  
  let function_definition (FD_aux (FD_function (_, _, x), (location, _))) =
    match x with
    | [ FCL_aux (Libsail.Ast.FCL_funcl (Libsail.Ast.Id_aux (Id identifier, _), _), _) ] -> identifier
    | _ -> not_yet_implemented [%here] location

  let type_definition (TD_aux (definition, (location, _))) =
    match definition with
    | TD_abbrev (Id_aux (Id identifier, _), _, _)     -> identifier
    | TD_record (Id_aux (Id identifier, _), _, _, _)  -> identifier
    | TD_variant (Id_aux (Id identifier, _), _, _, _) -> identifier
    | TD_enum (Id_aux (Id identifier, _), _, _)       -> identifier
    | TD_bitfield (Id_aux (Id identifier, _), _, _)   -> identifier
    | _                                               -> not_yet_implemented [%here] location
end


let ignore_definition (Libsail.Ast.DEF_aux (definition, _annotation)) =
  match definition with
  | Libsail.Ast.DEF_pragma (identifier, _, _) ->
     List.mem (get ignored_pragmas) identifier ~equal:String.equal
  | Libsail.Ast.DEF_fundef function_definition ->
     let identifier = Identifier.function_definition function_definition
     in
     List.mem (get ignored_functions) identifier ~equal:String.equal
  | Libsail.Ast.DEF_overload (_, _) -> get (ignore_overloads)
  | Libsail.Ast.DEF_type type_definition ->
     let identifier = Identifier.type_definition type_definition
     in
     List.mem (get ignored_types) identifier ~equal:String.equal
  | _ -> false
