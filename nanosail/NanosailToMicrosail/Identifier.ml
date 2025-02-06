open! ExtBase

let pp (identifier : Ast.Identifier.t) : PP.document =
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
