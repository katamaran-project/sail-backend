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

  
  let of_function_definition (FD_aux (FD_function (_, _, x), (location, _))) =
    match x with
    | [ FCL_aux (Libsail.Ast.FCL_funcl (Libsail.Ast.Id_aux (Id identifier, _), _), _) ] -> identifier
    | _ -> not_yet_implemented [%here] location

  let of_type_definition (TD_aux (definition, (location, _))) =
    match definition with
    | TD_abbrev (Id_aux (Id identifier, _), _, _)     -> identifier
    | TD_record (Id_aux (Id identifier, _), _, _, _)  -> identifier
    | TD_variant (Id_aux (Id identifier, _), _, _, _) -> identifier
    | TD_enum (Id_aux (Id identifier, _), _, _)       -> identifier
    | TD_bitfield (Id_aux (Id identifier, _), _, _)   -> identifier
    | _                                               -> not_yet_implemented [%here] location
end


let ignore_definition (Libsail.Ast.DEF_aux (definition, _annotation)) =
  let open Libsail.Ast
  in
  let member setting item =
    List.mem (get setting) item ~equal:String.equal
  in
  match definition with
  | DEF_pragma (identifier, _, _)  -> member ignored_pragmas   @@ identifier
  | DEF_fundef function_definition -> member ignored_functions @@ Identifier.of_function_definition function_definition
  | DEF_type type_definition       -> member ignored_types     @@ Identifier.of_type_definition type_definition
  | DEF_overload (_, _)            -> get (ignore_overloads)
  | _                              -> false
