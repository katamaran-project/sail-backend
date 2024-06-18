open Base


module Identifier           = Identifier
module NumericExpression    = NumericExpression
module Kind                 = Kind
module BinaryOperator       = BinaryOperator
module ExtendedType         = ExtendedType
module ExtendedFunctionType = ExtendedFunctionType
module Value                = Value
include Recursive
module Statement            = Statement
module Expression           = Expression
module Definition           = Definition


(*
  If given type is a tuple, collects all types inside of it in a list.
  If given type is not a tuple, simply return that type in a singleton list.
 *)
let tuple_to_list (t : Type.t) : Type.t list =
  match t with
  | Tuple ts -> ts
  | _        -> [ t ]


type program = {
    program_name : string;
    definitions  : (Sail.sail_definition * Definition.t) list   (* All translated definitions; original order preserved *)
  }


module Extract = struct
  let identity x = Some x

  let function_definition (definition : Definition.t) =
    match definition with
    | FunctionDefinition x -> Some x
    | _                    -> None

  let type_definition
      (of_kind    : Definition.Type.t -> 'a option)
      (definition : Definition.t                  )
    =
    match definition with
    | TypeDefinition x -> of_kind x
    | _                -> None

  let of_anything = Option.some

  let of_enum (type_definition : Definition.Type.t) =
    match type_definition with
    | Enum x -> Some x
    | _      -> None

  let of_variant (type_definition : Definition.Type.t) =
    match type_definition with
    | Variant x -> Some x
    | _         -> None

  let of_record (type_definition : Definition.Type.t) =
    match type_definition with
    | Record x -> Some x
    | _        -> None

  let of_abbreviation (type_definition : Definition.Type.t) =
    match type_definition with
    | Abbreviation x -> Some x
    | _              -> None

  let register_definition (definition : Definition.t) =
    match definition with
    | RegisterDefinition x -> Some x
    | _                    -> None

  let untranslated_definition (definition : Definition.t) =
    match definition with
    | UntranslatedDefinition x -> Some x
    | _                        -> None

  let ignored_definition (definition : Definition.t) =
    match definition with
    | IgnoredDefinition -> Some ()
    | _                 -> None

  let top_level_type_constraint_definition (definition : Definition.t) =
    match definition with
    | TopLevelTypeConstraintDefinition x -> Some x
    | _                                  -> None

  let value_definition (definition : Definition.t) =
    match definition with
    | ValueDefinition x -> Some x
    | _                 -> None
end


let select
    (extractor   : Definition.t -> 'a option                 )
    (definitions : (Sail.sail_definition * Definition.t) list)
  =
  let lift_extractor extractor (sail_definition, definition) =
    Option.map ~f:(fun def -> (sail_definition, def)) (extractor definition)
  in
  List.filter_map ~f:(lift_extractor extractor) definitions
