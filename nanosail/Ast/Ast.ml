open Base


module Identifier           = Identifier
module Numeric              = Numeric
module Kind                 = Kind
module UnaryOperator        = UnaryOperator
module BinaryOperator       = BinaryOperator
module Type                 = Nanotype
module TypeArgument         = TypeArgument
module ExtendedType         = ExtendedType
module ExtendedFunctionType = ExtendedFunctionType
module Value                = Value
module Statement            = Statement
module Expression           = Expression
module Definition           = Definition
module TypeQuantifier       = TypeQuantifier
module Normalize            = Normalize


(*
  If given type is a tuple, collects all types inside of it in a list.
  If given type is not a tuple, simply return that type in a singleton list.
 *)
let tuple_to_list (t : Type.t) : Type.t list =
  match t with
  | Tuple ts -> ts
  | _        -> [ t ]


type program = {
    definitions  : (Sail.sail_definition * Definition.t) list   (* All translated definitions; original order preserved *)
  }


let empty_program = { definitions = [] }
