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

    let to_fexpr (variant_definition : t) : FExpr.t =
      let constructor_to_fexpr (constructor : constructor) : FExpr.t =
        let constructor_identifier, constructor_field_types = constructor
        in
        let positional =
          [
            Identifier.to_fexpr constructor_identifier;
            FExpr.mk_list @@ List.map ~f:Type.to_fexpr constructor_field_types
          ]
        in
        FExpr.mk_application ~positional "Constructor"
      in

      let keyword =
        [
          ("identifier", Identifier.to_fexpr variant_definition.identifier);
          ("type_quantifier", TypeQuantifier.to_fexpr variant_definition.type_quantifier);
          ("constructors", FExpr.mk_list @@ List.map ~f:constructor_to_fexpr variant_definition.constructors);
        ]
      in
      FExpr.mk_application ~keyword "Def:Type:Variant"
  end

  module Enum = struct
    type t =
      {
        identifier : Identifier.t     ;
        cases      : Identifier.t list;
      }

    let to_fexpr (enum_definition : t) : FExpr.t =
      let keyword =
        [
          ("identifier", Identifier.to_fexpr enum_definition.identifier);
          ("cases", FExpr.mk_list @@ List.map ~f:Identifier.to_fexpr enum_definition.cases);
        ]
      in
      FExpr.mk_application ~keyword "Def:Type:Enum"
  end

  module Record = struct
    type t =
      {
        identifier      : Identifier.t                ;
        type_quantifier : TypeQuantifier.t            ;
        fields          : (Identifier.t * Type.t) list;
      }

    let to_fexpr (record_definition : t) : FExpr.t =
      let fexpr_of_field (field : Identifier.t * Type.t) : FExpr.t =
        let field_identifier, field_type = field
        in
        let keyword =
          [
            ("identifier", Identifier.to_fexpr field_identifier);
            ("type", Type.to_fexpr field_type);
          ]
        in
        FExpr.mk_application ~keyword "Field"
      in
      let keyword =
        [
          ("identifier", Identifier.to_fexpr record_definition.identifier);
          ("type_quantifier", TypeQuantifier.to_fexpr record_definition.type_quantifier);
          ("fields", FExpr.mk_list @@ List.map ~f:fexpr_of_field record_definition.fields);
        ]
      in
      FExpr.mk_application ~keyword "Def:Type:Record"
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

    let to_fexpr (type_abbreviation_definition : t) =
      let fexpr_of_abbreviation (abbreviation : type_abbreviation) : FExpr.t =
        match abbreviation with
        | NumericExpression (type_quantifier, expression) -> begin
            let keyword =
              [
                ("type_quantifier", TypeQuantifier.to_fexpr type_quantifier);
                ("expression", Numeric.Expression.to_fexpr expression);
              ]
            in
            FExpr.mk_application ~keyword "NE"
          end
        | NumericConstraint (type_quantifier, constr) -> begin
            let keyword =
              [
                ("type_quantifier", TypeQuantifier.to_fexpr type_quantifier);
                ("constraint", Numeric.Constraint.to_fexpr constr);
              ]
            in
            FExpr.mk_application ~keyword "NC"
          end
        | Alias (type_quantifier, aliased_type) -> begin
            let keyword =
              [
                ("type_quantifier", TypeQuantifier.to_fexpr type_quantifier);
                ("type", Type.to_fexpr aliased_type);
              ]
            in
            FExpr.mk_application ~keyword "Alias"
          end
      in
      let positional =
        [
          Identifier.to_fexpr type_abbreviation_definition.identifier;
          fexpr_of_abbreviation type_abbreviation_definition.abbreviation;
        ]
      in
      FExpr.mk_application ~positional "Def:Type:Abbreviation"
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

  let to_fexpr (type_definition : t) : FExpr.t =
    match type_definition with
    | Abbreviation abbreviation -> Abbreviation.to_fexpr abbreviation
    | Variant variant           -> Variant.to_fexpr variant
    | Enum enum                 -> Enum.to_fexpr enum
    | Record record             -> Record.to_fexpr record
end


module TopLevelTypeConstraint = struct
  type t =
    {
      identifier : Identifier.t;
    }

  let to_fexpr (top_level_type_constraint : t) : FExpr.t =
    let positional =
      [
        Identifier.to_fexpr top_level_type_constraint.identifier
      ]
    in
    FExpr.mk_application ~positional "Def:TopLevelTypeConstraint"
end


module Value = struct
  type t =
    {
      identifier : Identifier.t;
      value      : Value.t     ;
    }

  let to_fexpr (value_definition : t) : FExpr.t =
    let keyword =
      [
        ("identifier", Identifier.to_fexpr value_definition.identifier);
        ("value", Value.to_fexpr value_definition.value);
      ]
    in
    FExpr.mk_application ~keyword "Def:Value"
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

  module Selectors = struct
    class virtual ['a, 'b] selector = object(self)
      method virtual select   : 'a -> 'b option
      method virtual to_fexpr : FExpr.t
                                  
      method to_string : string =
        FExpr.to_string self#to_fexpr
    end
    

    class ['a] type_definition_selector (kind_selector : (Type.t, 'a) selector) = object
      inherit [t, 'a] selector

      method select (definition : t) =
        match definition with
        | TypeDefinition type_definition -> kind_selector#select type_definition
        | _                              -> None

      method to_fexpr : FExpr.t =
        let keyword =
          [ ("kind", kind_selector#to_fexpr) ]
        in
        FExpr.mk_application ~keyword "TypeSelector"
    end
    

    class virtual ['a, 'b] named_definition_selector (name : Identifier.t option) = object
      inherit ['a, 'b] selector

      method private matching_name (identifier : Identifier.t) : bool =
        match name with
        | Some name -> Identifier.equal name identifier
        | None      -> true

      method private fexpr_named_keywords : (string * FExpr.t) list =
        match name with
        | Some name -> [ ("named", Identifier.to_fexpr name) ]
        | None      -> []
    end
    

    class virtual ['a] kind_selector (name : Identifier.t option) = object
      inherit [Type.t, 'a] named_definition_selector name
    end
    

    class any_kind_selector (name : Identifier.t option) = object(self)
      inherit [Type.t] kind_selector name

      method select (type_definition : Type.t) : Type.t option =
        if
          self#matching_name @@ Type.identifier type_definition
        then
          Some type_definition
        else
          None

      method to_fexpr : FExpr.t =
        FExpr.mk_symbol "Any"
    end
    

    class enum_kind_selector (name : Identifier.t option) = object(self)
      inherit [Type.Enum.t] kind_selector name

      method select (type_definition : Type.t) : Type.Enum.t option =
        match type_definition with
        | Enum enum_definition when self#matching_name enum_definition.identifier -> Some enum_definition
        | _                                                                       -> None

      method to_fexpr : FExpr.t =
        let keyword =
          self#fexpr_named_keywords
        in
        FExpr.mk_application ~keyword "Enum"
    end
    
    
    class variant_kind_selector (name : Identifier.t option) = object(self)
      inherit [Type.Variant.t] kind_selector name

      method select (type_definition : Type.t) : Type.Variant.t option =
        match type_definition with
        | Variant variant_definition when self#matching_name variant_definition.identifier -> Some variant_definition
        | _                                                                                -> None

      method to_fexpr : FExpr.t =
        let keyword =
          self#fexpr_named_keywords
        in
        FExpr.mk_application ~keyword "Variant"
    end
    

    class record_kind_selector (name : Identifier.t option) = object(self)
      inherit [Type.Record.t] kind_selector name

      method select (type_definition : Type.t) : Type.Record.t option =
        match type_definition with
        | Record record_definition when self#matching_name record_definition.identifier -> Some record_definition
        | _                                                                             -> None

      method to_fexpr : FExpr.t =
        let keyword =
          self#fexpr_named_keywords
        in
        FExpr.mk_application ~keyword "Record"
    end
    

    class virtual ['a] abbreviation_subselector = object
      inherit [Type.Abbreviation.type_abbreviation, 'a] selector
    end

    
    class ['a] abbreviation_kind_selector
        (name        : Identifier.t option                        )
        (subselector : 'a abbreviation_subselector)
      =
      object(self)
        inherit [Identifier.t * 'a] kind_selector name

        method select (type_definition : Type.t) : (Identifier.t * 'a) option =
          match type_definition with
          | Abbreviation abbreviation_definition when self#matching_name abbreviation_definition.identifier -> begin
              Option.map
                ~f:(fun x -> (abbreviation_definition.identifier, x))
                (subselector#select abbreviation_definition.abbreviation) 
            end
          | _ -> None

        method to_fexpr : FExpr.t =
          let keyword =
            self#fexpr_named_keywords
          and positional =
            [ subselector#to_fexpr ]
          in
          FExpr.mk_application ~keyword ~positional "Abbreviation"
      end

    
    class alias_abbreviation_subselector = object
      inherit [TypeQuantifier.t * Nanotype.t] abbreviation_subselector

      method select (abbreviation_definition : Type.Abbreviation.type_abbreviation) : (TypeQuantifier.t * Nanotype.t) option =
        match abbreviation_definition with
        | Alias (type_quantifier, typ) -> Some (type_quantifier, typ)
        | _                            -> None

      method to_fexpr : FExpr.t =
        FExpr.mk_symbol "Alias"
    end
    

    class numeric_expression_abbreviation_subselector = object
      inherit [TypeQuantifier.t * Numeric.Expression.t] abbreviation_subselector

      method select (abbreviation_definition : Type.Abbreviation.type_abbreviation) : (TypeQuantifier.t * Numeric.Expression.t) option =
        match abbreviation_definition with
        | NumericExpression (type_quantifier, numeric_expression) -> Some (type_quantifier, numeric_expression)
        | _                                                       -> None

      method to_fexpr : FExpr.t =
        FExpr.mk_symbol "NumericExpression"
    end
    

    class register_selector (name : Identifier.t option) = object(self)
      inherit [t, Register.t] named_definition_selector name

      method select (definition : t) : Register.t option =
        match definition with
        | RegisterDefinition register_definition when self#matching_name register_definition.identifier -> Some register_definition
        | _                                                                                             -> None

      method to_fexpr : FExpr.t =
        let keyword =
          self#fexpr_named_keywords
        in
        FExpr.mk_application ~keyword "RegisterSelector"
    end

    
    class ignored_selector = object
      inherit [t, unit] selector

      method select (definition : t) : unit option =
        match definition with
        | IgnoredDefinition -> Some ()
        | _                 -> None

      method to_fexpr : FExpr.t =
        FExpr.mk_symbol "IgnoredSelector"
    end
    

    class top_level_type_constraint_definition_selector = object
      inherit [t, TopLevelTypeConstraint.t] selector

      method select (definition : t) : TopLevelTypeConstraint.t option =
        match definition with
        | TopLevelTypeConstraintDefinition top_level_type_constraint_definition -> Some top_level_type_constraint_definition
        | _                                                                     -> None

      method to_fexpr : FExpr.t =
        FExpr.mk_symbol "TopLevelTypeConstraintSelector"
    end

    
    class value_selector (name : Identifier.t option) = object(self)
      inherit [t, Value.t] named_definition_selector name

      method select (definition : t) : Value.t option =
        match definition with
        | ValueDefinition value_definition when self#matching_name value_definition.identifier -> Some value_definition
        | _                                                                                    -> None

      method to_fexpr : FExpr.t =
        let keyword =
          self#fexpr_named_keywords
        in
        FExpr.mk_application ~keyword "Selector:Value"
    end

    
    class untranslated_selector = object
      inherit [t, Untranslated.t] selector

      method select (definition : t) : Untranslated.t option =
        match definition with
        | UntranslatedDefinition untranslated_definition -> Some untranslated_definition
        | _                                              -> None


      method to_fexpr : FExpr.t =
        FExpr.mk_symbol "UntranslatedSelector"
    end


    class ['a, 'b] without_sail_selector (subselector : ('a, 'b) selector) = object
      inherit [Sail.sail_definition * 'a, 'b] selector

      method select (pair : Sail.sail_definition * 'a) : 'b option =
        let _, definition = pair
        in
        subselector#select definition

      method to_fexpr : FExpr.t =
        let positional =
          [ subselector#to_fexpr ]
        in
        FExpr.mk_application ~positional "Selector:WithoutSail"
    end

    class ['a, 'b] with_sail_selector (subselector : ('a, 'b) selector) = object
      inherit [Sail.sail_definition * 'a, Sail.sail_definition * 'b] selector

      method select (pair : Sail.sail_definition * 'a) : (Sail.sail_definition * 'b) option =
        let sail_definition, definition = pair
        in
        Option.map
          (subselector#select definition)
          ~f:(fun r -> (sail_definition, r))

      method to_fexpr : FExpr.t =
        let positional =
          [ subselector#to_fexpr ]
        in
        FExpr.mk_application ~positional "Selector:WithoutSail"
    end

    class function_selector (name : Identifier.t option) = object(self)
      inherit [t, Function.t] named_definition_selector name

      method select (definition : t) : Function.t option =
        match definition with
        | FunctionDefinition function_definition when self#matching_name function_definition.function_name -> Some function_definition
        | _                                                                                                -> None

      method to_fexpr : FExpr.t =
        let keyword =
          self#fexpr_named_keywords
        in
        FExpr.mk_application ~keyword "Selector:Function"
    end    
  end

  type ('a, 'b) selector = ('a, 'b) Selectors.selector

  (*
     Returns all definitions satisfying the selector.
  *)
  let select
      (selector    : ('a, 'b) selector)
      (definitions : 'a list          ) : 'b list
    =
    List.filter_map ~f:(fun definition -> selector#select definition) definitions

  let drop_sail_definitions (pairs : (Sail.sail_definition * 'a) list) : 'a list =
    List.map ~f:snd pairs
  
  let without_sail (subselector : ('a, 'b) selector) : (Sail.sail_definition * 'a, 'b) selector =
    new Selectors.without_sail_selector subselector

  let sail_accompanied (subselector : ('a, 'b) selector) : (Sail.sail_definition * 'a, Sail.sail_definition * 'b) selector =
    new Selectors.with_sail_selector subselector

  let identity x = Some x

  let function_definition ?(named : Identifier.t option) () : (t, Function.t) selector =
    new Selectors.function_selector named
  
  (* todo numeric_constraint subselector *)
  (* todo improve selector class names; add "definition" *)
  (* update selector fexpr *)
  
  let type_definition (of_kind : (Type.t, 'a) selector) : (t, 'a) selector =
    new Selectors.type_definition_selector of_kind
  
  let of_anything ?(named : Identifier.t option) () =
    new Selectors.any_kind_selector named

  let of_enum ?(named : Identifier.t option) () : (Type.t, Type.Enum.t) selector =
    new Selectors.enum_kind_selector named

  let of_variant ?(named : Identifier.t option) () : (Type.t, Type.Variant.t) selector =
    new Selectors.variant_kind_selector named
  
  let of_record ?(named : Identifier.t option) () : (Type.t, Type.Record.t) selector  =
    new Selectors.record_kind_selector named

  let of_abbreviation
      ?(named  : Identifier.t option                  )
      (of_type : 'a Selectors.abbreviation_subselector) : (Type.t, Identifier.t * 'a) selector
    =
    new Selectors.abbreviation_kind_selector named of_type

  let of_alias : (Type.Abbreviation.type_abbreviation, TypeQuantifier.t * Nanotype.t) selector =
    new Selectors.alias_abbreviation_subselector
        
  let of_numeric_expression : (Type.Abbreviation.type_abbreviation, TypeQuantifier.t * Numeric.Expression.t) selector =
    new Selectors.numeric_expression_abbreviation_subselector

  let register_definition ?(named : Identifier.t option) () : (t, Register.t) selector =
    new Selectors.register_selector named

  let untranslated_definition : (t, Untranslated.t) selector =
    new Selectors.untranslated_selector

  let ignored_definition : (t, unit) selector =
      new Selectors.ignored_selector
 
  let top_level_type_constraint_definition =
    new Selectors.top_level_type_constraint_definition_selector
  
  let value_definition ?(named : Identifier.t option) () : (t, Value.t) selector =
    new Selectors.value_selector named
end


let to_fexpr (definition : t) =
  match definition with
  | TopLevelTypeConstraintDefinition definition -> TopLevelTypeConstraint.to_fexpr definition
  | FunctionDefinition definition               -> Function.to_fexpr definition
  | TypeDefinition definition                   -> Type.to_fexpr definition
  | RegisterDefinition definition               -> Register.to_fexpr definition
  | UntranslatedDefinition definition           -> Untranslated.to_fexpr definition
  | ValueDefinition definition                  -> Value.to_fexpr definition
  | IgnoredDefinition                           -> FExpr.String "Def:Ignored"
