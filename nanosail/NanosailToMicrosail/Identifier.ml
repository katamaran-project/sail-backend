open Base


let pp (identifier : Ast.Identifier.t) : PP.document =
  PP.annotate [%here] @@ PP.string @@ Ast.Identifier.to_string identifier


let reified_enum_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Configuration.reified_enum_name identifier


let reified_record_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Configuration.reified_record_name identifier


let reified_variant_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Configuration.reified_variant_name identifier


let reified_variant_constructors_collection_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Configuration.reified_variant_constructors_collection_name identifier


let reified_variant_constructor_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Configuration.reified_variant_constructor_name identifier


let record_constructor_name (identifier : Ast.Identifier.t) : Ast.Identifier.t =
  Configuration.record_constructor_name identifier
