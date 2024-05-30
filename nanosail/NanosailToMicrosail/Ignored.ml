open! Base


let generate (ignored_definition : Sail.sail_definition) =
  PPSail.pp_sail_definition ignored_definition
