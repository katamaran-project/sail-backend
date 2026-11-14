open ExtBase

let pp (identifier : Ast.Identifier.t) : PP.t =
  PP.annotate [%here] @@ PP.string @@ Ast.Identifier.to_string identifier

(* todo make functions below configurable *)

let reified_enum_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.update (fun x -> "E" ^ String.lowercase x) identifier


let reified_record_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.update (fun x -> "R" ^ String.lowercase x) identifier


let reified_variant_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.update (fun x -> "U" ^ String.lowercase x) identifier


let reified_variant_constructors_collection_name (variant_identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.add_suffix "Constructor" variant_identifier


let reified_variant_constructor_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.update (fun x -> "K" ^ String.lowercase x) identifier


let record_constructor_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Ast.Identifier.add_prefix "Mk" identifier


(*
   Name of the Rocq projection for a record field.

   Rocq record projections are global, whereas Sail allows different structs to share field names
   (e.g., CHERIoT's Capability and EncCapability both have address, B, T and reserved).
   Prefixing each projection with the name of its record keeps the names unique and stable:
   adding a struct never renames the projections of another one.
   Only the Rocq projections are renamed; µSail refers to fields by their Sail names (as strings).
*)
let record_field_projection_name
    (record_identifier : Ast.Identifier.t)
    (field_identifier  : Ast.Identifier.t) : Ast.Identifier.t
  =
  Ast.Identifier.add_prefix (Ast.Identifier.to_string record_identifier ^ "_") field_identifier
