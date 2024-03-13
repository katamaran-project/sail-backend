open! Base


let generate (ignored_definition : Ast.sail_definition) =
  Sail.pp_sail_definition ignored_definition
