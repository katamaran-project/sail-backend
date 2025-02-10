open! ExtBase


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
module Simplify             = Simplify
module Renaming             = Renaming


type program = {
  definitions  : (Sail.sail_definition * Definition.t) list   (* List of all translated definitions; original order preserved *)
}


let empty_program = { definitions = [] }
