(*
   In a first phase, Sail code is translated into an intermediate language called nanosail.
   During a second phase, nanosail is translated into muSail.

   This module (and its submodules) contain all data types and functionality
   related to this intermediate language.

   Design-wise, nanosail is supposed to be a distilled version of Sail, i.e.,
   it contains only those bits of information that are relevant to
   the translation in muSail.
*)

module Identifier           = Identifier
module Numeric              = Numeric
module Kind                 = Kind
module UnaryOperator        = UnaryOperator
module BinaryOperator       = BinaryOperator
module Type                 = Type
module TypeArgument         = TypeArgument
module ExtendedType         = ExtendedType
module ExtendedFunctionType = ExtendedFunctionType
module Value                = Value
module Statement            = Statement
module Expression           = Expression
module Definition           = Definition
module TypeQuantifier       = TypeQuantifier
module Renaming             = Renaming
module Program              = Program
