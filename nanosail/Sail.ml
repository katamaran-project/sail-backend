(*

   Some Sail related auxiliary definitions

*)

open ExtBase
open Libsail.Ast

type type_annotation = Libsail.Type_check.tannot

type definition_annotation = Libsail.Type_check.env Libsail.Ast.def_annot

type sail_definition = (type_annotation, Libsail.Type_check.env) Libsail.Ast.def

type ast = (Libsail.Type_check.tannot, Libsail.Type_check.env) Libsail.Ast_defs.ast


let string_of_location (location : Libsail.Parse_ast.l) = StringOf.Sail.location location


(*
   Extracts name as string; fails on operator name
*) (* todo function already exists somewhere else *)
let string_of_id (id : id) : string =
  let Id_aux (id, _loc) = id
  in
  match id with
  | Id s       -> s
  | Operator _ -> failwith "operator names not supported"


(*
   Determines the name of a function
*)
let identifier_of_function_definition (function_definition : 'a fundef) : string =
  let FD_aux (FD_function (_, _, x), (location, _)) = function_definition
  in
  match x with
  | [ FCL_aux (Libsail.Ast.FCL_funcl (Libsail.Ast.Id_aux (Id identifier, _), _), _) ] -> identifier
  | _ -> failwith (Printf.sprintf "wanted to extract function name from function definition; failed because I didn't recognize structure, at %s" (string_of_location location))


let identifier_of_top_level_type_constraint (top_level_type_constraint : 'a val_spec) : string =
  let VS_aux (unwrapped_top_level_type_constraint, _annotation) = top_level_type_constraint
  in
  let VS_val_spec (_type_scheme, identifier, _extern) = unwrapped_top_level_type_constraint
  in
  match identifier with
  | Id_aux (Id name, _)       -> name
  | Id_aux (Operator name, _) -> name


let identifier_of_type_definition (TD_aux (definition, (_location, _))) =
  match definition with
  | TD_abbrev (id, _, _)     -> string_of_id id
  | TD_record (id, _, _, _)  -> string_of_id id
  | TD_variant (id, _, _, _) -> string_of_id id
  | TD_enum (id, _, _)       -> string_of_id id
  | TD_bitfield (id, _, _)   -> string_of_id id
  | TD_abstract (id, _)      -> string_of_id id


let rec identifier_of_pattern (pattern : 'a pat) : string =
  let P_aux (pattern, _) = pattern
  in
  let not_supported (position : Lexing.position) =
    let error_message = Printf.sprintf "not supported (%s)" @@ StringOf.OCaml.position position
    in
    failwith error_message
  in
  match pattern with
  | P_id identifier             -> string_of_id identifier
  | P_typ (_, pattern)          -> identifier_of_pattern pattern
  | P_lit _                     -> not_supported [%here]
  | P_wild                      -> not_supported [%here]
  | P_or (_, _)                 -> not_supported [%here]
  | P_not _                     -> not_supported [%here]
  | P_as (_, _)                 -> not_supported [%here]
  | P_var (_, _)                -> not_supported [%here]
  | P_app (_, _)                -> not_supported [%here]
  | P_vector _                  -> not_supported [%here]
  | P_vector_concat _           -> not_supported [%here]
  | P_vector_subrange (_, _, _) -> not_supported [%here]
  | P_tuple _                   -> not_supported [%here]
  | P_list _                    -> not_supported [%here]
  | P_cons (_, _)               -> not_supported [%here]
  | P_string_append _           -> not_supported [%here]
  | P_struct (_, _)             -> not_supported [%here]


let identifier_of_value_definition (value_definition : Libsail.Type_check.tannot letbind) : string =
  let LB_aux (LB_val (pattern, E_aux (_, _)), (_location2, _type_annotation)) = value_definition
  in
  identifier_of_pattern pattern


(*
   Some wildcards are rewritten as named binders, e.g., _ becomes g__41.
   This function attempts to recognize these.
*)
let is_named_wildcard (sail_identifier : id) : bool =
  let Id_aux (unwrapped_sail_identifier, _location) = sail_identifier
  in
  match unwrapped_sail_identifier with
  | Id name -> String.is_prefix name ~prefix:"g__"
  | Operator _ -> false
