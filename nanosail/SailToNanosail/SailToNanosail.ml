module Translation        = Translation
module Sanitation         = Sanitation
module TranslationContext = TranslationContext
module Prelude            = Prelude
module Identifier         = Identifier
module Numeric            = Numeric
module Basics             = Basics
module Nanotype           = Nanotype
module Expression         = Expression
module Function           = Function
module TypeQuantifier     = TypeQuantifier
module TypeAbbreviation   = TypeAbbreviation
module Enum               = Enum
module Variant            = Variant
module Record             = Record
module TypeDefinition     = TypeDefinition


let translate             = Translation.translate
let coqify_identifiers    = Sanitation.coqify_identifiers
