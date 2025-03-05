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
module Renaming             = Renaming


type program = {
  definitions          : (Sail.sail_definition * Definition.t) list;  (* list of all translated definitions; original order preserved *)
  polymorphic_argtypes : Type.t list list Identifier.Map.t;           (* maps polymorphic functions to the argument types *)
}


let empty_program =
  {
    definitions          = [];
    polymorphic_argtypes = Identifier.Map.empty;
  }
