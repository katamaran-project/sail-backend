open! ExtBase
open Monads.Notations.Star(GenerationContext)


module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(* Name for the inductive type listing all enum types *)
let enums_inductive_type_identifier = Ast.Identifier.mk "Enums"


let pp_enum_definition (enum_definition : Ast.Definition.Type.Enum.t) : PP.document GC.t =
  GC.generation_block [%here] "Enum Definition" begin
    let identifier = Identifier.pp enum_definition.identifier
    and typ = Identifier.pp @@ Ast.Identifier.mk "Set"
    in
    GC.pp_inductive_type identifier typ (fun add_constructor ->
        GC.iter ~f:add_constructor @@ List.map ~f:Identifier.pp enum_definition.cases
      )
  end


let generate_tags (enum_definitions : (Sail.sail_definition * Ast.Definition.Type.Enum.t) list) : PP.document GC.t =
  let enum_definitions =
    Ast.Definition.Select.drop_sail_definitions enum_definitions
  in
  let identifier = Identifier.pp enums_inductive_type_identifier
  and typ = PP.string "Set"
  and tag_of_enum (enum_definition : Ast.Definition.Type.Enum.t) =
    let id = Identifier.reified_enum_name enum_definition.identifier
    in
    Identifier.pp id
  in
  GC.block begin
    GC.pp_inductive_type
      identifier
      typ
      (
        fun add_constructor -> begin
            let* () = add_constructor @@ Identifier.pp Registers.regname_tag
            in
            GC.iter
              ~f:(fun enum_identifier ->
                  add_constructor @@ tag_of_enum enum_identifier
                )
              enum_definitions
          end
      )
  end


let eqdec_identifiers_for (enum_definition : Ast.Definition.Type.Enum.t) : Ast.Identifier.t list =
  [ enum_definition.identifier ]


let extra_eqdec_identifiers () =
  [ enums_inductive_type_identifier ]


let no_confusion_identifiers_for (enum_definition : Ast.Definition.Type.Enum.t) : Ast.Identifier.t list =
  [ enum_definition.identifier ]


let extra_no_confusion_identifiers () =
  [ enums_inductive_type_identifier ]


let generate_enum_finiteness (enum_definition  : Ast.Definition.Type.Enum.t) =
  let identifier = Identifier.pp @@ enum_definition.identifier
  and type_name  = Identifier.pp @@ enum_definition.identifier
  and values     = List.map ~f:Identifier.pp enum_definition.cases
  in
  Coq.pp_finite_instance ~identifier ~type_name ~values


let generate_finiteness (enum_definitions : (Sail.sail_definition * Ast.Definition.Type.Enum.t) list) =
  let definitions = Ast.Definition.Select.drop_sail_definitions enum_definitions
  in
  List.map ~f:generate_enum_finiteness definitions
