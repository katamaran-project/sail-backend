open ExtBase
open Monads.Notations.Star(GenerationContext)

module GC = struct
  include GenerationContext
  include Monads.Util.Make(GenerationContext)
end


(* Name for the inductive type listing all variant/union types *)
let records_inductive_type_identifier = Ast.Identifier.mk "Records"

let derive_constructor_from_identifier identifier =
  Identifier.record_constructor_name identifier


(*
   A record family: struct S('m) = { value : bits(8 * 'm) } becomes Record S (kid__m : nat),
   whose tag in Records takes the parameter too (Rs (kid__m : nat)).
   Only one numeric parameter, as union width families: Records' EqDec injects into N * nat,
   and Katamaran's records cannot be indexed by a Ty.
*)
let is_family (record_definition : Ast.Definition.Type.Record.t) : bool =
  let Ast.TypeQuantifier.TypeQuantifier items = record_definition.type_quantifier
  in
  match items with
  | [ (_, Int) ]                        -> true
  | [ (_, Type) ] | [ (_, Bool) ]       -> false
  | []                                  -> false
  | _ :: _ :: _                         -> false


let parameter_identifiers (record_definition : Ast.Definition.Type.Record.t) : Ast.Identifier.t list =
  if is_family record_definition
  then begin
    let Ast.TypeQuantifier.TypeQuantifier items = record_definition.type_quantifier
    in
    List.map ~f:fst items
  end
  else []


(* name applied to the parameters of the record: Rs kid__m, S kid__m *)
let applied_to_parameters (record_definition : Ast.Definition.Type.Record.t) (name : PP.t) : PP.t =
  PP.separate_horizontally ~separator:PP.space (name :: List.map ~f:Identifier.pp (parameter_identifiers record_definition))


(* The tag of the record in Records, as a pattern binding its parameters *)
let tag_pattern (record_definition : Ast.Definition.Type.Record.t) : PP.t =
  applied_to_parameters record_definition (Identifier.pp @@ Identifier.reified_record_name record_definition.identifier)


(* The Rocq type of the record at the parameters bound by tag_pattern *)
let denoted_type (record_definition : Ast.Definition.Type.Record.t) : PP.t =
  applied_to_parameters record_definition (Identifier.pp record_definition.identifier)


let pp_record_definition (record_definition : Ast.Definition.Type.Record.t) : PP.t GC.t =
  GC.generation_block [%here] "Record definition" begin
    let generate_field field_identifier field_type =
      let* field_type' = Type.coq_type_of_nanotype field_type
      in
      let projection_identifier =
        Identifier.record_field_projection_name record_definition.identifier field_identifier
      in
      GC.return (Identifier.pp projection_identifier, field_type')
    in
    let identifier  = Identifier.pp record_definition.identifier
    and type_name   = Identifier.pp @@ Ast.Identifier.mk "Set"
    and constructor_identifier = derive_constructor_from_identifier record_definition.identifier
    and parameters  =
      List.map (parameter_identifiers record_definition) ~f:(fun parameter ->
          PP.(surround parens @@ separate_horizontally ~separator:space [ Identifier.pp parameter; colon; string "nat" ]))
    in
    let constructor = Identifier.pp constructor_identifier
    in
    let* fields     = GC.map ~f:(Fn.uncurry generate_field) record_definition.fields
    in
    let record = Coq.pp_record ~identifier ~parameters ~type_name ~constructor ~fields
    in
    (* the constructor and projections of a family take its parameters implicitly: MkS v, S_value r *)
    match parameter_identifiers record_definition with
    | []         -> GC.return record
    | parameters -> begin
        let arguments_for (function_identifier : Ast.Identifier.t) : PP.t =
          PP.string @@ Printf.sprintf "#[global] Arguments %s {%s}."
            (Ast.Identifier.to_string function_identifier)
            (String.concat ~sep:" " @@ List.map ~f:Ast.Identifier.to_string parameters)
        and projection_identifiers =
          List.map record_definition.fields ~f:(fun (field_identifier, _) ->
              Identifier.record_field_projection_name record_definition.identifier field_identifier)
        in
        GC.return @@ PP.vertical (record :: List.map ~f:arguments_for (constructor_identifier :: projection_identifiers))
      end
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
  and tag_parameters (record_definition : Ast.Definition.Type.Record.t) : PP.t =
    PP.separate_horizontally ~separator:PP.space @@ List.map (parameter_identifiers record_definition) ~f:(fun identifier ->
        PP.(surround parens @@ separate_horizontally ~separator:space [ Identifier.pp identifier; colon; string "nat" ])
      )
  in
  GC.block begin
    GC.pp_inductive_type
      identifier
      typ
      (fun add_constructor ->
         GC.iter
           ~f:(fun record_definition ->
               add_constructor ~parameters:(tag_parameters record_definition) @@ tag_of_record record_definition
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


let no_confusion_identifiers_for (record_definition : Ast.Definition.Type.Record.t) : Ast.Identifier.t list =
  [ record_definition.identifier ]

