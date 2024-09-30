open Base

module Big_int = Nat_big_num

module Sanitation = Sanitation

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
  let rec nanotype_of_sail_type (S.Typ_aux (typ, location)) : Ast.Type.t TC.t =
  (*
    Types are representing as strings in Sail.
  *)
    let rec nanotype_of_identifier (identifier : S.id) : Ast.Type.t TC.t =
      let Id_aux (_, location) = identifier
      in
      let* identifier' = Identifier.translate_identifier [%here] identifier
      in
      let id_as_string = Ast.Identifier.string_of identifier'
      in
      match id_as_string with
      | "bool"      -> TC.return @@ Ast.Type.Bool
      | "int"       -> TC.return @@ Ast.Type.Int
      | "unit"      -> TC.return @@ Ast.Type.Unit
      | "string"    -> TC.return @@ Ast.Type.String
      | "atom"      -> TC.fail [%here] "Type atom should be intercepted higher up"
      | "atom_bool" -> TC.fail [%here] "Type atom_bool should be intercepted higher up"
      | "bits"      -> TC.fail [%here] "Type bits should be intercepted higher up"
      | _           -> begin
          let* type_definition : Ast.Definition.Type.t option = TC.lookup_type_definition identifier'
          in
          match type_definition with
          | Some (Abbreviation { identifier; abbreviation }) -> begin
              let _ = identifier (* todo remove this *)
              in
              match abbreviation with
              | NumericExpression (_, _)     -> TC.not_yet_implemented [%here] location
              | NumericConstraint (_, _)     -> TC.not_yet_implemented [%here] location
              | Alias (type_quantifier, typ) -> begin
                  match type_quantifier with
                  | []   -> TC.return @@ Ast.Type.Alias (identifier', typ)
                  | _::_ -> TC.not_yet_implemented [%here] location
                end
            end
          | Some (Variant _) -> TC.return @@ Ast.Type.Variant identifier'
          | Some (Record _)  -> TC.return @@ Ast.Type.Record identifier'
          | Some (Enum _)    -> TC.return @@ Ast.Type.Enum identifier'
          | None             -> TC.fail [%here] @@ Printf.sprintf "Unknown type %s" id_as_string
        end  
  
  (*
    Sail represents types with parameters with Typ_app (id, type_args).
    This function translates these to their corresponding nanotype.
  *)
    and nanotype_of_type_constructor
        (identifier     : S.id          )
        (type_arguments : S.typ_arg list) : Ast.Type.t TC.t
      =
      let* type_arguments' = TC.map ~f:translate_type_argument type_arguments
      and* identifier'     = Identifier.translate_identifier [%here] identifier
      in
      match (Ast.Identifier.string_of identifier'), type_arguments' with
      | "list" , [ Type t ]    -> TC.return @@ Ast.Type.List t
      | "atom", [ _ ]          -> TC.return Ast.Type.Int
      | "atom_bool", [ _ ]     -> TC.return Ast.Type.Bool
      | "bits", args           -> nanotype_of_bitvector args
      | _, _                   -> begin
          let* constructor = nanotype_of_identifier identifier
          in
          TC.return @@ Ast.Type.Application (constructor, type_arguments')
        end

    and nanotype_of_bitvector (args : Ast.TypeArgument.t list) : Ast.Type.t TC.t =
      match args with
      | [ Ast.TypeArgument.NumericExpression numeric_expression ] -> TC.return @@ Ast.Type.Bitvector numeric_expression
      | [ _ ]                                                     -> TC.fail [%here] "Bitvector argument expected to be numeric expression"
      | _                                                         -> TC.fail [%here] "Bitvector should receive exactly one argument"
  
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
      match items' with
      | [ t1; t2 ] -> TC.return @@ Ast.Type.Product (t1, t2) (* represent pairs using products *)
      | _          -> TC.return @@ Ast.Type.Tuple items'

    in
    match typ with
    | Typ_tuple items                 -> nanotype_of_tuple items
    | Typ_id id                       -> nanotype_of_identifier id
    | Typ_app (identifier, type_args) -> nanotype_of_type_constructor identifier type_args
    | Typ_exist (ids, nc, typ)        -> nanotype_of_existential ids nc typ
    | Typ_internal_unknown            -> TC.not_yet_implemented [%here] location
    | Typ_var _                       -> TC.not_yet_implemented [%here] location
    | Typ_fn (_, _)                   -> TC.not_yet_implemented [%here] location
    | Typ_bidir (_, _)                -> TC.not_yet_implemented [%here] location


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
        (factory : Ast.Numeric.Expression.t -> Ast.Numeric.Expression.t -> Ast.Numeric.Expression.t)
        (left    : S.nexp                                                                          )
        (right   : S.nexp                                                                          ) : Ast.Numeric.Expression.t TC.t
      =
      let* left'  = translate_numeric_expression left
      and* right' = translate_numeric_expression right
      in
      TC.return @@ factory left' right'
    in

    let translate_sum   = translate_binary_operation @@ fun l r -> Add   (l, r)
    and translate_minus = translate_binary_operation @@ fun l r -> Minus (l, r)
    and translate_times = translate_binary_operation @@ fun l r -> Times (l, r)

    in
    let S.Nexp_aux (unwrapped_numeric_expression, numexp_location) = numeric_expression
    in
    match unwrapped_numeric_expression with
    | Nexp_constant constant                     -> TC.return @@ Ast.Numeric.Expression.Constant constant
    | Nexp_var (Kid_aux (Var string, _location)) -> TC.return @@ Ast.Numeric.Expression.Var (Ast.Identifier.mk string)
    | Nexp_times (x, y)                          -> translate_times x y
    | Nexp_sum (x, y)                            -> translate_sum x y
    | Nexp_minus (x, y)                          -> translate_minus x y
    | Nexp_neg x  -> begin
        let* x' = translate_numeric_expression x
        in
        TC.return @@ Ast.Numeric.Expression.Neg x'
      end
    | Nexp_id identifier -> begin
        let* identifier' = Identifier.translate_identifier [%here] identifier
        in
        TC.return @@ Ast.Numeric.Expression.Id identifier'
      end
    | Nexp_exp _      -> TC.not_yet_implemented [%here] numexp_location
    | Nexp_app (_, _) -> TC.not_yet_implemented [%here] numexp_location

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

    let translate_equal      = translate_comparison       @@ fun l r -> Equal      (l, r)
    and translate_not_equal  = translate_comparison       @@ fun l r -> NotEqual   (l, r)
    and translate_bounded_ge = translate_comparison       @@ fun l r -> BoundedGE  (l, r)
    and translate_bounded_gt = translate_comparison       @@ fun l r -> BoundedGT  (l, r)
    and translate_bounded_le = translate_comparison       @@ fun l r -> BoundedLE  (l, r)
    and translate_bounded_lt = translate_comparison       @@ fun l r -> BoundedLT  (l, r)
    and translate_or         = translate_binary_operation @@ fun l r -> Or         (l, r)
    and translate_and        = translate_binary_operation @@ fun l r -> And        (l, r)
    in
    let S.NC_aux (unwrapped_numeric_constraint, _numeric_constraint_location) = numeric_constraint
    in
    match unwrapped_numeric_constraint with
    | S.NC_equal (x, y)                          -> translate_equal      x y
    | S.NC_not_equal (x, y)                      -> translate_not_equal  x y
    | S.NC_bounded_ge (x, y)                     -> translate_bounded_ge x y
    | S.NC_bounded_gt (x, y)                     -> translate_bounded_gt x y
    | S.NC_bounded_le (x, y)                     -> translate_bounded_le x y
    | S.NC_bounded_lt (x, y)                     -> translate_bounded_lt x y
    | S.NC_app (function_id, arguments)          -> translate_application function_id arguments
    | S.NC_or (x, y)                             -> translate_or x y
    | S.NC_and (x, y)                            -> translate_and x y
    | S.NC_set (Kid_aux (Var kind_id, _loc), ns) -> TC.return @@ Ast.Numeric.Constraint.Set (Ast.Identifier.mk kind_id, ns)
    | S.NC_var (Kid_aux (Var kind_id, _loc))     -> TC.return @@ Ast.Numeric.Constraint.Var (Ast.Identifier.mk kind_id)
    | S.NC_true                                  -> TC.return @@ Ast.Numeric.Constraint.True
    | S.NC_false                                 -> TC.return @@ Ast.Numeric.Constraint.False
end
