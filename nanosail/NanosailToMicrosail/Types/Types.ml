open Base
open PPrint
open Ast
open Identifier
open Monads.Notations.Star(AnnotationContext)

module AC = AnnotationContext
module PP = PPrint

(* cleanup *)
module Variants = Variants
module TypeAbbreviations = TypeAbbreviations
module Enums = Enums


module Records = struct
  let generate (record_definition : record_definition) : document AC.t =
    let generate_field field_identifier field_type =
      let* field_type' = Nanotype.coq_type_of_nanotype field_type
      in
      AC.return (string field_identifier, field_type')
    in
    let* identifier = AC.return @@ PP.string @@ record_definition.identifier
    and* type_name = AC.return @@ PP.string "Set"
    and* constructor = AC.return @@ PP.string @@ "Mk" ^ record_definition.identifier (* todo: allow custom name *)
    and* fields = AC.map ~f:(Auxlib.uncurry generate_field) record_definition.fields
    in
    AC.return @@ Coq.record ~identifier ~type_name ~constructor ~fields
end


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
