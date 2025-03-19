open Monads.Notations.Star(GenerationContext)

(* todo define mli file in order to hide module aliases *)
module GC                = GenerationContext

module Variants          = Variants
module TypeAbbreviations = TypeAbbreviations
module Enums             = Enums
module Records           = Records


let pp_type_definition
      (original        : Sail.sail_definition )
      (type_definition : Ast.Definition.Type.t) : PP.document GC.t
  =
  let* pp_type_def =
    match type_definition with
    | Abbreviation abbrev -> TypeAbbreviations.pp_type_abbreviation abbrev
    | Enum enum           -> Enums.pp_enum_definition enum
    | Variant variant     -> Variants.pp_variant_definition variant
    | Record record       -> Records.pp_record_definition record
  in
  GC.block begin
    let* () = GC.add_original_definition original
    in
    GC.return @@ pp_type_def
  end
