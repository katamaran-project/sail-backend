open! ExtBase

module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Anf
end

module TC = TranslationContext
open Monads.Notations.Star(TC)


module rec Nanotype : sig
  val nanotype_of_sail_type   : S.typ -> Ast.Type.t TC.t
  val translate_type_argument : S.typ_arg -> Ast.TypeArgument.t TC.t
end = struct
  let rec nanotype_of_sail_type (typ : S.typ) : Ast.Type.t TC.t =
    TC.translation_block [%here] (PP.string @@ "Translating type " ^ StringOf.Sail.typ typ) begin
      let (S.Typ_aux (unwrapped_type, location)) = typ
      in
      (*
        Types are representing as strings in Sail.
      *)
      let rec nanotype_of_identifier (identifier : S.id) : Ast.Type.t TC.t =
        let Id_aux (_, location) = identifier
        in
        let* identifier' = Identifier.translate_identifier [%here] identifier
        in
        let id_as_string = Ast.Identifier.to_string identifier'
        in
        match id_as_string with
        | "bool"      -> TC.return @@ Ast.Type.Bool
        | "int"       -> TC.return @@ Ast.Type.Int
        | "unit"      -> TC.return @@ Ast.Type.Unit
        | "string"    -> TC.return @@ Ast.Type.String
        | "bit"       -> TC.return @@ Ast.Type.Bit
        | "atom"      -> TC.fail [%here] "Type atom should be intercepted higher up"
        | "atom_bool" -> TC.fail [%here] "Type atom_bool should be intercepted higher up"
        | "bits"      -> TC.fail [%here] "Type bits should be intercepted higher up"
        | "bitvector" -> TC.fail [%here] "Type bitvector should be intercepted higher up"
        | _           -> begin
            let* type_definition : Ast.Definition.Type.t option =
              TC.lookup_definition_opt (Ast.Definition.Select.(type_definition @@ of_anything_named identifier'))
            in
            match type_definition with
            | Some (Abbreviation { identifier; abbreviation }) -> begin
                let _ = identifier (* keeps away unused var warning/error *)
                in
                match abbreviation with
                | NumericExpression (_, _)     -> TC.not_yet_implemented [%here] location
                | NumericConstraint (_, _)     -> TC.not_yet_implemented [%here] location
                | Alias (type_quantifier, typ) -> begin
                    let Ast.TypeQuantifier.TypeQuantifier items = type_quantifier
                    in
                    match items with
                    | []   -> TC.return @@ Ast.Type.Alias (identifier', typ)
                    | _::_ -> TC.not_yet_implemented [%here] location
                  end
              end
            | Some (Variant _) -> TC.return @@ Ast.Type.Variant identifier'
            | Some (Record _)  -> TC.return @@ Ast.Type.Record identifier'
            | Some (Enum _)    -> TC.return @@ Ast.Type.Enum identifier'
            | _                -> TC.not_yet_implemented ~message:(Printf.sprintf "unknown type %s" (Ast.Identifier.to_string identifier')) [%here] location
          end

      (*
        Sail represents types with parameters with Typ_app (id, type_args).
        This function translates these to their corresponding nanotype.
      *)
      and nanotype_of_application
          (identifier     : S.id          )
          (type_arguments : S.typ_arg list) : Ast.Type.t TC.t
        =
        let* type_arguments' = TC.map ~f:translate_type_argument type_arguments
        and* identifier'     = Identifier.translate_identifier [%here] identifier
        in
        match Ast.Identifier.to_string identifier' with
        | "list"      -> nanotype_of_list type_arguments'
        | "atom"      -> nanotype_of_atom type_arguments'
        | "atom_bool" -> nanotype_of_atom_bool type_arguments'
        | "bits"      -> nanotype_of_bits type_arguments'
        | "bitvector" -> nanotype_of_bitvector type_arguments'
        | "range"     -> nanotype_of_range type_arguments'
        | "itself"    -> nanotype_of_itself type_arguments'
        | _           -> begin
            let* constructor = nanotype_of_identifier identifier
            in
            TC.return @@ Ast.Type.Application (constructor, type_arguments')
          end

      and nanotype_of_itself (args : Ast.TypeArgument.t list) : Ast.Type.t TC.t =
        (*
           itself(x) seems to be a singleton type, e.g., itself(50) has as sole value 50.

           todo probably needs to always return Ast.Type.Int
           for now, it's left unimplemented so as to catch more occurrences in the wild
           to give us confirmation
        *)
        match args with
        | [ type_argument ] -> begin
            match type_argument with
            | NumericExpression numeric_expression -> begin
                match numeric_expression with
                | Constant _                -> TC.return Ast.Type.Int
                | Var _                     -> TC.return Ast.Type.Int
                | BinaryOperation (_, _, _) -> TC.not_yet_implemented [%here] location
                | Neg _                     -> TC.not_yet_implemented [%here] location
                | PowerOf2 _                -> TC.not_yet_implemented [%here] location
                | Id _                      -> TC.not_yet_implemented [%here] location
              end
            | Type _                        -> TC.not_yet_implemented [%here] location
            | Bool _                        -> TC.not_yet_implemented [%here] location
          end
        | _ -> TC.fail [%here] "type 'itself' expected to receive exactly one parameter"

      and nanotype_of_atom (args : Ast.TypeArgument.t list) : Ast.Type.t TC.t =
        match args with
        | [ _ ] -> begin
            let* () = TC.log [%here] Logging.debug @@ lazy (PP.format "simplifying %s to int" @@ StringOf.Sail.typ typ)
            in
            TC.return Ast.Type.Int
          end
        | _     -> TC.fail [%here] "atom expected to have exactly one argument"

      and nanotype_of_atom_bool (args : Ast.TypeArgument.t list) : Ast.Type.t TC.t =
        match args with
        | [ _ ] -> TC.return Ast.Type.Bool
        | _     -> TC.fail [%here] "atom_bool expected to have exactly one argument"

      and nanotype_of_range (args : Ast.TypeArgument.t list) : Ast.Type.t TC.t =
        match args with
        | [ Ast.TypeArgument.NumericExpression lower_bound; Ast.TypeArgument.NumericExpression upper_bound ] -> begin
            TC.return @@ Ast.Type.Range (lower_bound, upper_bound)
          end
        | _                            -> begin
            let message =
              let args' =
                String.concat ~sep:", " @@ List.map ~f:(fun arg -> FExpr.to_string @@ Ast.TypeArgument.to_fexpr arg) args
              in
              Printf.sprintf "range should have exactly two arguments of type numeric_expression; instead got %s" args'
            in
            TC.fail [%here] message
          end

      and nanotype_of_list (args : Ast.TypeArgument.t list) : Ast.Type.t TC.t =
        match args with
        | [ Ast.TypeArgument.Type t ] -> TC.return @@ Ast.Type.List t
        | [ _ ]                       -> TC.fail [%here] "list argument expected to be type"
        | _                           -> TC.fail [%here] "list should receive exactly one argument"

      and nanotype_of_bits (args : Ast.TypeArgument.t list) : Ast.Type.t TC.t =
        match args with
        | [ Ast.TypeArgument.NumericExpression numeric_expression ] -> TC.return @@ Ast.Type.Bitvector numeric_expression
        | [ _ ]                                                     -> TC.fail [%here] "bits argument expected to be numeric expression"
        | _                                                         -> TC.fail [%here] "bits should receive exactly one argument"

      (*
        The type "bitvector" looks a lot like "bits" and they might be the same, but we set up a separate function just in case.
        The purpose of this function is therefore more a reminder that there are two different types for bitvectors.
      *)
      and nanotype_of_bitvector (args : Ast.TypeArgument.t list) : Ast.Type.t TC.t =
        nanotype_of_bits args

      and nanotype_of_existential
          (ids         : Libsail.Ast.kinded_id list)
          (constraints : Libsail.Ast.n_constraint  )
          (typ         : Libsail.Ast.typ           ) : Ast.Type.t TC.t
        =
        let ids'                = String.concat ~sep:", " @@ List.map ~f:Libsail.Ast_util.string_of_kinded_id ids
        and numeric_constraint' = StringOf.Sail.n_constraint constraints
        and typ'                = Libsail.Ast_util.string_of_typ typ
        in
        begin
          if
            Configuration.(get print_warnings)
          then
            Stdio.printf "Encountered Typ_exist\nKinded ids: %s\nNumeric constraint: %s\nType: %s\n\n" ids' numeric_constraint' typ'
        end;
        nanotype_of_sail_type typ

      and nanotype_of_tuple (items : Libsail.Ast.typ list) : Ast.Type.t TC.t =
        let* items' = TC.map ~f:nanotype_of_sail_type items
        in
        TC.return @@ Ast.Type.Tuple items'

      in
      match unwrapped_type with
      | Typ_tuple items                 -> nanotype_of_tuple items
      | Typ_id id                       -> nanotype_of_identifier id
      | Typ_app (identifier, type_args) -> nanotype_of_application identifier type_args
      | Typ_exist (ids, nc, typ)        -> nanotype_of_existential ids nc typ
      | Typ_internal_unknown            -> TC.not_yet_implemented [%here] location
      | Typ_var _                       -> TC.not_yet_implemented [%here] location
      | Typ_fn (_, _)                   -> TC.not_yet_implemented [%here] location
      | Typ_bidir (_, _)                -> TC.not_yet_implemented [%here] location
    end

  and translate_type_argument (type_argument : Libsail.Ast.typ_arg) : Ast.TypeArgument.t TC.t =
    let S.A_aux (unwrapped_type_argument, _location) = type_argument
    in
    match unwrapped_type_argument with
    | A_nexp e -> begin
        let* e' = Numeric.translate_numeric_expression e
        in
        TC.return @@ Ast.TypeArgument.NumericExpression e'
      end
    | A_typ t  -> begin
        let* t' = nanotype_of_sail_type t
        in
        TC.return @@ Ast.TypeArgument.Type t'
      end
    | A_bool b -> begin
        let* b' = Numeric.translate_numeric_constraint b
        in
        TC.return @@ Ast.TypeArgument.Bool b'
      end
end

and Numeric : sig
  val translate_numeric_expression : Libsail.Ast.nexp -> Ast.Numeric.Expression.t TC.t
  val translate_numeric_constraint : S.n_constraint -> Ast.Definition.NumericConstraint.t TC.t
end = struct
  let rec translate_numeric_expression (numeric_expression : Libsail.Ast.nexp) : Ast.Numeric.Expression.t TC.t =
    let translate_binary_operation
        (operator : Ast.Numeric.Expression.binop)
        (left    : S.nexp                       )
        (right   : S.nexp                       ) : Ast.Numeric.Expression.t TC.t
      =
      TC.translation_block [%here] (PP.string "Translating binary operation") begin
        let* left'  = translate_numeric_expression left
        and* right' = translate_numeric_expression right
        in
        TC.return @@ Ast.Numeric.Expression.BinaryOperation (operator, left', right')
      end
    in

    let translate_sum      = translate_binary_operation Add
    and translate_minus    = translate_binary_operation Sub
    and translate_times    = translate_binary_operation Mul
    and translate_division = translate_binary_operation Div
    and translate_modulo   = translate_binary_operation Mod

    and translate_constant (constant : Z.t) : Ast.Numeric.Expression.t TC.t =
      TC.return @@ Ast.Numeric.Expression.Constant constant

    and translate_variable (id : S.kid) : Ast.Numeric.Expression.t TC.t =
      let Kid_aux (Var string, _location) = id
      in
      TC.return @@ Ast.Numeric.Expression.Var (Ast.Identifier.mk string)

    and translate_negation (numeric_expression : S.nexp) =
      let* numeric_expression' = translate_numeric_expression numeric_expression
      in
      TC.return @@ Ast.Numeric.Expression.Neg numeric_expression'

    and translate_identifier (identifier : S.id) =
      let* identifier' = Identifier.translate_identifier [%here] identifier
      in
      TC.return @@ Ast.Numeric.Expression.Id identifier'

    and translate_exponentiation (exponent : S.nexp) : Ast.Numeric.Expression.t TC.t =
      let* exponent' = translate_numeric_expression exponent
      in
      TC.return @@ Ast.Numeric.Expression.PowerOf2 exponent'

    in
    TC.translation_block [%here] (PP.string "Translating numeric expression") begin
      let S.Nexp_aux (unwrapped_numeric_expression, numexp_location) =
        numeric_expression
      in
      match unwrapped_numeric_expression with
      | Nexp_constant constant      -> translate_constant constant
      | Nexp_var kid                -> translate_variable kid
      | Nexp_times (x, y)           -> translate_times x y
      | Nexp_sum (x, y)             -> translate_sum x y
      | Nexp_minus (x, y)           -> translate_minus x y
      | Nexp_neg negated            -> translate_negation negated
      | Nexp_id identifier          -> translate_identifier identifier
      | Nexp_exp exponent           -> translate_exponentiation exponent
      | Nexp_app (identifier, arguments) -> begin
          let S.Id_aux (unwrapped_identifier, location) = identifier
          in
          match unwrapped_identifier, arguments with
          | Id "div", [x; y] -> translate_division x y
          | Id "mod", [x; y] -> translate_modulo x y
          | _             -> TC.not_yet_implemented ~message:(StringOf.Sail.nexp numeric_expression) [%here] location
        end
      | Nexp_if (_, _, _)           -> TC.not_yet_implemented [%here] numexp_location
    end

  and translate_numeric_constraint (numeric_constraint : Libsail.Ast.n_constraint) : Ast.Numeric.Constraint.t TC.t =
    let translate_comparison
        (factory : Ast.Numeric.Expression.t -> Ast.Numeric.Expression.t -> Ast.Numeric.Constraint.t)
        (left    : Libsail.Ast.nexp                                                                )
        (right   : Libsail.Ast.nexp                                                                ) : Ast.Numeric.Constraint.t TC.t
      =
      let* left'  = translate_numeric_expression left
      and* right' = translate_numeric_expression right
      in
      TC.return @@ factory left' right'

    and translate_binary_operation
        (factory : Ast.Numeric.Constraint.t -> Ast.Numeric.Constraint.t -> Ast.Numeric.Constraint.t)
        (left    : Libsail.Ast.n_constraint                                                        )
        (right   : Libsail.Ast.n_constraint                                                        ) : Ast.Numeric.Constraint.t TC.t
      =
      let* left'  = translate_numeric_constraint left
      and* right' = translate_numeric_constraint right
      in
      TC.return @@ factory left' right'

    and translate_application
        (function_identifier : Libsail.Ast.id          )
        (arguments           : Libsail.Ast.typ_arg list) : Ast.Numeric.Constraint.t TC.t
      =
      let* function_identifier' =
        Identifier.translate_identifier [%here] function_identifier
      and* arguments' =
        TC.map ~f:Nanotype.translate_type_argument arguments
      in
      TC.return @@ Ast.Numeric.Constraint.App (function_identifier', arguments')
    in

    let S.NC_aux (unwrapped_numeric_constraint, location) = numeric_constraint
    in
    let translate_equal (x : S.typ_arg) (y : S.typ_arg) =
      let* x' = Nanotype.translate_type_argument x
      and* y' = Nanotype.translate_type_argument y
      in
      TC.return @@ Ast.Numeric.Constraint.Equal (x', y')

    and translate_not_equal (x : S.typ_arg) (y : S.typ_arg) =
      let* x' = Nanotype.translate_type_argument x
      and* y' = Nanotype.translate_type_argument y
      in
      TC.return @@ Ast.Numeric.Constraint.NotEqual (x', y')

    and translate_ge            = translate_comparison       @@ fun l r -> GreaterThanOrEqualTo (l, r)
    and translate_gt            = translate_comparison       @@ fun l r -> GreaterThan          (l, r)
    and translate_le            = translate_comparison       @@ fun l r -> LessThanOrEqualTo    (l, r)
    and translate_lt            = translate_comparison       @@ fun l r -> LessThan             (l, r)
    and translate_or            = translate_binary_operation @@ fun l r -> Or                   (l, r)
    and translate_and           = translate_binary_operation @@ fun l r -> And                  (l, r)

    in
    TC.translation_block [%here] (PP.string "Translating numeric constraint") begin
      match unwrapped_numeric_constraint with
      | S.NC_equal (x, y)                          -> translate_equal     x y
      | S.NC_not_equal (x, y)                      -> translate_not_equal x y
      | S.NC_ge (x, y)                             -> translate_ge        x y
      | S.NC_gt (x, y)                             -> translate_gt        x y
      | S.NC_le (x, y)                             -> translate_le        x y
      | S.NC_lt (x, y)                             -> translate_lt        x y
      | S.NC_or (x, y)                             -> translate_or        x y
      | S.NC_and (x, y)                            -> translate_and       x y
      | S.NC_app (function_id, arguments)          -> translate_application function_id arguments
      | S.NC_var (Kid_aux (Var kind_id, _loc))     -> TC.return @@ Ast.Numeric.Constraint.Var (Ast.Identifier.mk kind_id)
      | S.NC_true                                  -> TC.return @@ Ast.Numeric.Constraint.True
      | S.NC_false                                 -> TC.return @@ Ast.Numeric.Constraint.False
      | S.NC_set (_, _)                            -> TC.not_yet_implemented [%here] location
      | S.NC_id _                                  -> TC.not_yet_implemented [%here] location
    end
end
