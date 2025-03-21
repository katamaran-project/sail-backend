(*
   Base is a standard library for OCaml developed by Jane Street (https://github.com/janestreet/base).
   BaseExt is our own extension of this library.
*)

module List       = List
module Fn         = Fn
module Tuple      = Tuple
module String     = String
module Sequence   = Sequence
module Map        = Map
module Option     = Base.Option
module Int        = Base.Int
module Bool       = Base.Bool
module Char       = Base.Char
module Comparator = Base.Comparator
module Printf     = Base.Printf


let (<.) = Fn.compose
