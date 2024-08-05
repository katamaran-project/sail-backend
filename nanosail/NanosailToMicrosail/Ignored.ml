open! Base

module GC = GenerationContext
  

let generate (ignored_definition : Sail.sail_definition) : PP.document GC.t =
  GC.return @@ GC.pp_sail_definition ignored_definition
