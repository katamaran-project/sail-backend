open Base
include Recursive


type type_quantifier_item = Identifier.t * Kind.t


type type_quantifier = type_quantifier_item list


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
  type t =
    {
      identifier    : Identifier.t  ;
      typ           : Type.t        ;
      initial_value : Value.t option;
    }

  let to_fexpr (register_definition : t) : FExpr.t =
    let keyword =
      [
        ("identifier", Identifier.to_fexpr register_definition.identifier);
        ("type", Type.to_fexpr register_definition.typ);
      ]
    in
    FExpr.mk_application ~keyword "Def:Register"
end


module Type = struct
  module Variant = struct
    type t =
      {
        identifier      : Identifier.t    ;
        type_quantifier : type_quantifier ;
        constructors    : constructor list;
      }

    and constructor = (Identifier.t * Type.t list)
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
        type_quantifier : type_quantifier             ;
        fields          : (Identifier.t * Type.t) list;
      }
  end

  module Abbreviation = struct
    type type_abbreviation = (* todo find better name *)
      | NumericExpression of type_quantifier * NumericExpression.t
      | NumericConstraint of type_quantifier * NumericConstraint.t
      | Alias             of type_quantifier * Type.t

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


type top_level_type_constraint_definition =
  {
    identifier : Identifier.t;
  }


type value_definition =
  {
    identifier : Identifier.t;
    value      : Value.t     ;
  }


type t =
  | TopLevelTypeConstraintDefinition of top_level_type_constraint_definition
  | FunctionDefinition               of Function.t
  | TypeDefinition                   of Type.t
  | RegisterDefinition               of Register.t
  | UntranslatedDefinition           of Untranslated.t
  | ValueDefinition                  of value_definition
  | IgnoredDefinition


module Select = struct
  let select
      (extractor   : t -> 'a option                 )
      (definitions : (Sail.sail_definition * t) list)
    =
    let lift_extractor extractor (sail_definition, definition) =
      Option.map ~f:(fun def -> (sail_definition, def)) (extractor definition)
    in
    List.filter_map ~f:(lift_extractor extractor) definitions


  let identity x = Some x

  let function_definition (definition : t) =
    match definition with
    | FunctionDefinition x -> Some x
    | _                    -> None

  let type_definition
      (of_kind    : Type.t -> 'a option)
      (definition : t                  )
    =
    match definition with
    | TypeDefinition x -> of_kind x
    | _                -> None

  let of_anything = Option.some

  let of_enum (type_definition : Type.t) =
    match type_definition with
    | Enum x -> Some x
    | _      -> None

  let of_variant (type_definition : Type.t) =
    match type_definition with
    | Variant x -> Some x
    | _         -> None

  let of_record (type_definition : Type.t) =
    match type_definition with
    | Record x -> Some x
    | _        -> None

  let of_abbreviation (type_definition : Type.t) =
    match type_definition with
    | Abbreviation x -> Some x
    | _              -> None

  let of_alias (type_definition : Type.t) =
    match type_definition with
    | Type.Abbreviation { identifier; abbreviation } -> begin
        match abbreviation with
        | NumericExpression (_, _) -> None
        | NumericConstraint (_, _) -> None
        | Alias (quant, t)         -> Some (identifier, quant, t)
      end
    | _ -> None

  let register_definition (definition : t) =
    match definition with
    | RegisterDefinition x -> Some x
    | _                    -> None

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

  let value_definition (definition : t) =
    match definition with
    | ValueDefinition x -> Some x
    | _                 -> None
end
