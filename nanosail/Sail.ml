open Libsail.Ast
open NYI


let identifier_of_function_definition (FD_aux (FD_function (_, _, x), (location, _))) =
  match x with
  | [ FCL_aux (Libsail.Ast.FCL_funcl (Libsail.Ast.Id_aux (Id identifier, _), _), _) ] -> identifier
  | _ -> not_yet_implemented [%here] location


let identifier_of_type_definition (TD_aux (definition, (location, _))) =
  match definition with
  | TD_abbrev (Id_aux (Id identifier, _), _, _)     -> identifier
  | TD_record (Id_aux (Id identifier, _), _, _, _)  -> identifier
  | TD_variant (Id_aux (Id identifier, _), _, _, _) -> identifier
  | TD_enum (Id_aux (Id identifier, _), _, _)       -> identifier
  | TD_bitfield (Id_aux (Id identifier, _), _, _)   -> identifier
  | _                                               -> not_yet_implemented [%here] location
