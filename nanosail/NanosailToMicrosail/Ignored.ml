open Base

module GC = GenerationContext


let generate (ignored_definition : Sail.sail_definition) : PP.t GC.t =
  GC.return @@ PP.annotate [%here] @@ PPSail.pp_sail_definition ignored_definition
