module Translation = Translation
module Sanitation = Sanitation
module TranslationContext = TranslationContext

let translate          = Translation.translate
let coqify_identifiers = Sanitation.coqify_identifiers
