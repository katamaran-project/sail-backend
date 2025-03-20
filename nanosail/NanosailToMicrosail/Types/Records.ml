open! ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(* Name for the inductive type listing all variant/union types *)
let records_inductive_type_identifier = Ast.Identifier.mk "Records"

let derive_constructor_from_identifier identifier =
  Identifier.record_constructor_name identifier


let pp_record_definition (record_definition : Ast.Definition.Type.Record.t) : PP.t GC.t =
  GC.generation_block [%here] "Record definition" begin
    let generate_field field_identifier field_type =
      let* field_type' = Nanotype.coq_type_of_nanotype field_type
      in
      GC.return (Identifier.pp field_identifier, field_type')
    in
    let identifier  = Identifier.pp record_definition.identifier
    and type_name   = Identifier.pp @@ Ast.Identifier.mk "Set"
    and constructor = Identifier.pp @@ derive_constructor_from_identifier record_definition.identifier
    in
    let* fields     = GC.map ~f:(Fn.uncurry generate_field) record_definition.fields
    in
    GC.return @@ Coq.pp_record ~identifier ~type_name ~constructor ~fields
  end

let generate_tags (record_definitions : (Sail.sail_definition * Ast.Definition.Type.Record.t) list) =
  let record_definitions =
    Ast.Definition.Select.drop_sail_definitions record_definitions
  in
  let identifier = Identifier.pp records_inductive_type_identifier
  and typ = PP.string "Set"
  and tag_of_record (record_definition : Ast.Definition.Type.Record.t) =
    let id = Identifier.reified_record_name record_definition.identifier
    in
    Identifier.pp id
  in
  GC.block begin
    GC.pp_inductive_type
      identifier
      typ
      (fun add_constructor ->
         GC.iter
           ~f:(fun record_identifier ->
               add_constructor @@ tag_of_record record_identifier
             )
           record_definitions
      )
  end


let generate_tag_match
    ?(scope               : string option                                      = None)
    ~(matched_identifier  : Ast.Identifier.t                                         )
    ~(record_definitions  : Ast.Definition.Type.Record.t list                        )
    ~(record_case_handler : Ast.Definition.Type.Record.t -> (PP.t * PP.t) GC.t       ) () : PP.t GC.t
  =
  let scope = Option.map ~f:PP.string scope
  in
  let* cases = GC.map ~f:record_case_handler record_definitions
  in
  GC.return @@ Coq.pp_match ~scope (Identifier.pp matched_identifier) cases


let eqdec_identifiers_for (record_definition : Ast.Definition.Type.Record.t) : Ast.Identifier.t list =
  [ record_definition.identifier ]


let extra_eqdec_identifiers () =
  [ records_inductive_type_identifier ]


let no_confusion_identifiers_for (record_definition : Ast.Definition.Type.Record.t) : Ast.Identifier.t list =
  [ record_definition.identifier ]


let extra_no_confusion_identifiers () =
  [ records_inductive_type_identifier ]
