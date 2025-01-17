open Base
include Recursive


module FunctionType = struct
  type t =
    {
      parameters  : (Identifier.t * Type.t) list;
      return_type : Type.t
    }

  let to_fexpr (function_type_definition : t) : FExpr.t =
    let parameters' =
      let parameter_to_fexpr
            (identifier : Identifier.t)
            (typ        : Type.t      ) : FExpr.t
        =
        FExpr.mk_application ~positional:[Identifier.to_fexpr identifier; Type.to_fexpr typ] "Parameter"
      in
      FExpr.mk_list @@ List.map ~f:(Auxlib.uncurry parameter_to_fexpr) function_type_definition.parameters

    and return_type' =
      Type.to_fexpr function_type_definition.return_type

    in
    let keyword =
      [
        ("parameters", parameters');
        ("return_type", return_type');
      ]
    in
    FExpr.mk_application ~keyword "Def:FunctionType"
end


module Function = struct
  type t =
    {
      function_name          : Identifier.t;
      function_type          : FunctionType.t;
      extended_function_type : ExtendedFunctionType.t;
      function_body          : Statement.t;
    }

  let to_fexpr (function_definition : t) : FExpr.t =
    let function_name' =
      Identifier.to_fexpr function_definition.function_name

    and function_type' =
      FunctionType.to_fexpr function_definition.function_type

    and extended_function_type' =
      FExpr.mk_string "TODO"

    and function_body' =
      Statement.to_fexpr function_definition.function_body

    in
    let keyword =
      [
        ("name", function_name');
        ("type", function_type');
        ("extended_type", extended_function_type');
        ("body", function_body');
      ]
    in
    FExpr.mk_application ~keyword "Def:Function"
end


module Untranslated = struct
  type t =
    {
      filename      : string             ;
      line_number   : int                ;
      sail_location : Libsail.Parse_ast.l;
      message       : string option      ;
    }

  let to_fexpr (untranslated_definition : t) =
    let message' =
      match untranslated_definition.message with
      | Some message -> FExpr.mk_string message
      | None         -> FExpr.mk_nil
    in
    let keyword =
      [
        ("filename", FExpr.mk_string untranslated_definition.filename);
        ("line_number", FExpr.mk_int untranslated_definition.line_number);
        ("sail_location", FExpr.mk_string @@ Sail.string_of_location untranslated_definition.sail_location);
        ("message", message')
      ]
    in
    FExpr.mk_application ~keyword "Def:Untranslated"
end


module Register = struct
  type initial_value =
    | NoneSpecified
    | Specified     of Value.t
    | RawSpecified  of string        (* used in case we were not able to translate the value; contains a string representation of the Libsail value *)

  type t =
    {
      identifier    : Identifier.t ;
      typ           : Type.t       ;
      initial_value : initial_value;
    }

  let to_fexpr (register_definition : t) : FExpr.t =
    let keyword =
      [
        ("identifier", Identifier.to_fexpr register_definition.identifier);
        ("type", Type.to_fexpr register_definition.typ);
        (* todo : add initial value *)
      ]
    in
    FExpr.mk_application ~keyword "Def:Register"
end


module Type = struct
  module Variant = struct
    type t =
      {
        identifier      : Identifier.t    ;
        type_quantifier : TypeQuantifier.t;
        constructors    : constructor list;
      }

    and constructor = (Identifier.t * Type.t list)

    let find_constructor_field_types
        (definition : t           )
        (target     : Identifier.t) : Type.t list option
      =
      let is_right_constructor (constructor : constructor) : bool =
        let constructor_identifier, _ = constructor
        in
        Identifier.equal constructor_identifier target
      in
      match List.find definition.constructors ~f:is_right_constructor with
      | Some (_, fields) -> Some fields
      | None             -> None
  end

  module Enum = struct
    type t =
      {
        identifier : Identifier.t     ;
        cases      : Identifier.t list;
      }
  end

  module Record = struct
    type t =
      {
        identifier      : Identifier.t                ;
        type_quantifier : TypeQuantifier.t            ;
        fields          : (Identifier.t * Type.t) list;
      }
  end

  module Abbreviation = struct
    type type_abbreviation = (* todo find better name *)
      | NumericExpression of TypeQuantifier.t * NumericExpression.t
      | NumericConstraint of TypeQuantifier.t * NumericConstraint.t
      | Alias             of TypeQuantifier.t * Type.t

    type t =
      {
        identifier   : Identifier.t     ;
        abbreviation : type_abbreviation;
      }
  end

  type t =
    | Abbreviation of Abbreviation.t
    | Variant      of Variant.t
    | Enum         of Enum.t
    | Record       of Record.t

  let identifier (type_definition : t) : Identifier.t =
    match type_definition with
    | Abbreviation x -> x.identifier
    | Variant x      -> x.identifier
    | Enum x         -> x.identifier
    | Record x       -> x.identifier
end


module TopLevelTypeConstraint = struct
  type t =
    {
      identifier : Identifier.t;
    }
end


module Value = struct
  type t =
    {
      identifier : Identifier.t;
      value      : Value.t     ;
    }
end


type t =
  | TopLevelTypeConstraintDefinition of TopLevelTypeConstraint.t
  | FunctionDefinition               of Function.t
  | TypeDefinition                   of Type.t
  | RegisterDefinition               of Register.t
  | UntranslatedDefinition           of Untranslated.t
  | ValueDefinition                  of Value.t
  | IgnoredDefinition


module Select = struct
  type ('a, 'b) selector = 'a -> 'b option
  
  (*
     Returns all definitions satisfying the selector.
  *)
  let select
      (selector    : ('a, 'b) selector)
      (definitions : 'a list          )
    =
    List.filter_map ~f:selector definitions

  let drop_sail_definitions (pairs : (Sail.sail_definition * 'a) list) : 'a list =
    List.map ~f:snd pairs

  let sail_accompanied
      (selector : ('a, 'b) selector        )
      (pair     : Sail.sail_definition * 'a)
    =
    let sail_definition, nano_definition = pair
    in
    match selector nano_definition with
    | Some x -> Some (sail_definition, x)
    | None   -> None
  
  let identity x = Some x

  let function_definition (definition : t) =
    match definition with
    | FunctionDefinition x -> Some x
    | _                    -> None

  let type_definition (of_kind : (Type.t, 'a) selector) : (t, 'a) selector =
    let selector (definition : t) =
      match definition with
      | TypeDefinition x -> of_kind x
      | _                -> None
    in
    selector

  (*
     Helper function used in selectors
  *)
  let is_named
      (identifier  : Identifier.t       )
      (identifier' : Identifier.t option) : bool
    =
    match identifier' with
    | Some identifier' -> Identifier.equal identifier identifier'
    | None             -> true

  let of_anything
      ?(named           : Identifier.t option)
       (type_definition : Type.t             ) : Type.t option
    =
    if
      is_named (Type.identifier type_definition) named
    then
      Some type_definition
    else
      None

  let of_enum ?(named : Identifier.t option) (type_definition : Type.t) =
    match type_definition with
    | Enum x when is_named x.identifier named -> Some x
    | _                                       -> None

  let of_variant ?(named : Identifier.t option) (type_definition : Type.t) =
    match type_definition with
    | Variant x when is_named x.identifier named -> Some x
    | _                                          -> None

  let of_record ?(named : Identifier.t option) (type_definition : Type.t) =
    match type_definition with
    | Record x when is_named x.identifier named -> Some x
    | _                                         -> None

  let of_abbreviation ?(named : Identifier.t option)(type_definition : Type.t) =
    match type_definition with
    | Abbreviation x when is_named x.identifier named -> Some x
    | _                                               -> None

  let of_alias ?(named : Identifier.t option) (type_definition : Type.t) =
    match type_definition with
    | Type.Abbreviation { identifier; abbreviation } when is_named identifier named -> begin
        match abbreviation with
        | NumericExpression (_, _) -> None
        | NumericConstraint (_, _) -> None
        | Alias (quant, t)         -> Some (identifier, quant, t)
      end
    | _ -> None

  let register_definition ?(named : Identifier.t option) (definition : t) =
    match definition with
    | RegisterDefinition x when is_named x.identifier named -> Some x
    | _                                                     -> None

  let untranslated_definition (definition : t) =
    match definition with
    | UntranslatedDefinition x -> Some x
    | _                        -> None

  let ignored_definition (definition : t) =
    match definition with
    | IgnoredDefinition -> Some ()
    | _                 -> None

  let top_level_type_constraint_definition (definition : t) =
    match definition with
    | TopLevelTypeConstraintDefinition x -> Some x
    | _                                  -> None

  let value_definition ?(named : Identifier.t option) (definition : t) =
    match definition with
    | ValueDefinition def when is_named def.identifier named -> Some def
    | _                                                      -> None
end
