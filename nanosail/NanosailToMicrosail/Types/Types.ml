open PPrint
open Ast
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module PP = PPrint

module Variants          = Variants
module TypeAbbreviations = TypeAbbreviations
module Enums             = Enums
module Records           = Records


let pp_type_definition
      (original        : sail_definition)
      (type_definition : type_definition) : document
  =
  let document =
    match type_definition with
    | TD_abbreviation abbrev -> TypeAbbreviations.generate abbrev
    | TD_enum enum           -> Enums.generate enum
    | TD_variant variant     -> Variants.generate variant
    | TD_record record       -> Records.generate record
  in
  Coq.annotate_with_original_definition original (Coq.annotate document)
