open Monads.Notations.Star(GenerationContext)

(* todo define mli file in order to hide module aliases *)
module GC                = GenerationContext

module Variants          = Variants
module TypeAbbreviations = TypeAbbreviations
module Enums             = Enums
module Records           = Records
module ExtendedType      = ExtendedType


let pp_type_definition
      (original        : Sail.sail_definition )
      (type_definition : Ast.Definition.Type.t) : PP.document GC.t
  =
  let* document =
    match type_definition with
    | Abbreviation abbrev -> TypeAbbreviations.generate abbrev
    | Enum enum           -> Enums.generate enum
    | Variant variant     -> Variants.generate variant
    | Record record       -> Records.generate record
  in
  GC.block begin
    let* () = GC.add_original_definition original
    in
    GC.return @@ document
  end
