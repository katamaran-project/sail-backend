open Monads.Notations.Star(AnnotationContext)

module AC                = AnnotationContext
module Variants          = Variants
module TypeAbbreviations = TypeAbbreviations
module Enums             = Enums
module Records           = Records
module ExtendedType      = ExtendedType


let pp_type_definition
      (original        : Sail.sail_definition )
      (type_definition : Ast.Definition.Type.t) : PP.document
  =
  let document =
    match type_definition with
    | Abbreviation abbrev -> TypeAbbreviations.generate abbrev
    | Enum enum           -> Enums.generate enum
    | Variant variant     -> AC.return @@ Variants.generate variant
    | Record record       -> Records.generate record
  in
  Coq.annotate_with_original_definition original @@ Coq.annotate document
