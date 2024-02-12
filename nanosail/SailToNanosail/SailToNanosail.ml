module Translation        = Translation
module Sanitation         = Sanitation
module TranslationContext = TranslationContext
module Prelude            = Prelude
module Identifier         = Identifier
module Numeric            = Numeric
module Basics             = Basics
module Nanotype           = Nanotype
module Expression         = Expression

let translate             = Translation.translate
let coqify_identifiers    = Sanitation.coqify_identifiers
