module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module N  = Ast
module TC = TranslationContext

open Base
open Monads.Notations.Star(TC)


(*
  Returns the parameter types as a list
*)
let unpack_parameter_types (parameter_bindings : N.type_annotation Libsail.Ast.pat) : S.typ list TC.t =
  let S.P_aux (_unwrapped_parameter_bindings, parameter_bindings_annotation) = parameter_bindings
  in
  let parameter_bundle_type = Libsail.Type_check.typ_of_annot parameter_bindings_annotation
  in
  let Typ_aux (unwrapped_parameter_bundle_type, _parameter_bundle_type_location) = parameter_bundle_type
  in
  match unwrapped_parameter_bundle_type with
  | S.Typ_tuple ts -> TC.return ts
  | _              -> TC.return [ parameter_bundle_type ]


(*
  Focuses on parameter types.
  Atoms must only contain a single identifier as their type argument.
*)
let extended_parameter_type_of_sail_type (sail_type : S.typ) : N.ExtendedType.Parameter.t TC.t =
  let S.Typ_aux (unwrapped_sail_type, sail_type_location) = sail_type
  in
  match unwrapped_sail_type with
   | S.Typ_internal_unknown -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_var _            -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_fn (_, _)        -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_bidir (_, _)     -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_tuple _          -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_exist (_, _, _)  -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_id _             -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_app (identifier, type_arguments) -> begin
       let Id_aux (unwrapped_identifier, identifier_location) = identifier
       in
       match unwrapped_identifier with
       | Id "atom" -> begin
           match type_arguments with
           | [ type_argument ] -> begin
               let S.A_aux (unwrapped_type_argument, type_argument_location) = type_argument
               in
               match unwrapped_type_argument with
                | S.A_typ _                     -> TC.not_yet_implemented [%here] type_argument_location
                | S.A_bool _                    -> TC.not_yet_implemented [%here] type_argument_location
                | S.A_nexp numerical_expression -> begin
                    let S.Nexp_aux (unwrapped_numerical_expression, numerical_expression_location) = numerical_expression
                    in
                    match unwrapped_numerical_expression with
                     | S.Nexp_id _         -> TC.not_yet_implemented [%here] numerical_expression_location
                     | S.Nexp_constant _   -> TC.not_yet_implemented [%here] numerical_expression_location
                     | S.Nexp_app (_, _)   -> TC.not_yet_implemented [%here] numerical_expression_location
                     | S.Nexp_times (_, _) -> TC.not_yet_implemented [%here] numerical_expression_location
                     | S.Nexp_sum (_, _)   -> TC.not_yet_implemented [%here] numerical_expression_location
                     | S.Nexp_minus (_, _) -> TC.not_yet_implemented [%here] numerical_expression_location
                     | S.Nexp_exp _        -> TC.not_yet_implemented [%here] numerical_expression_location
                     | S.Nexp_neg _        -> TC.not_yet_implemented [%here] numerical_expression_location
                     | S.Nexp_var kid      -> begin
                         let S.Kid_aux (Var unwrapped_kid, _kid_location) = kid
                         in
                         TC.return @@ N.ExtendedType.Parameter.Int (Some unwrapped_kid)
                       end
                  end
             end
           | _ -> TC.not_yet_implemented ~message:"Unexpected number of type arguments (should be exactly one)" [%here] sail_type_location
         end
       | Id string -> begin
           let message =
             Printf.sprintf "Unknown type %s" string
           in
           TC.not_yet_implemented ~message [%here] identifier_location
         end
       | Operator _ -> TC.not_yet_implemented [%here] identifier_location
     end


let rec int_expression_of_sail_numeric_expression (numeric_expression : S.nexp) : N.ExtendedType.IntExpression.t TC.t =
  let binary_operation
        (factory : N.ExtendedType.IntExpression.t -> N.ExtendedType.IntExpression.t -> N.ExtendedType.IntExpression.t)
        (left    : S.nexp                                                                                            )
        (right   : S.nexp                                                                                            ) : N.ExtendedType.IntExpression.t TC.t
    =
    let* left'  = int_expression_of_sail_numeric_expression left
    and* right' = int_expression_of_sail_numeric_expression right
    in
    TC.return @@ factory left' right'
  in
  let S.Nexp_aux (unwrapped_numeric_expression, numeric_expression_location) = numeric_expression
  in
  match unwrapped_numeric_expression with
   | S.Nexp_id _                -> TC.not_yet_implemented [%here] numeric_expression_location
   | S.Nexp_app (_, _)          -> TC.not_yet_implemented [%here] numeric_expression_location
   | S.Nexp_exp _               -> TC.not_yet_implemented [%here] numeric_expression_location
   | S.Nexp_neg _               -> TC.not_yet_implemented [%here] numeric_expression_location
   | S.Nexp_constant n          -> TC.return @@ N.ExtendedType.IntExpression.Constant n
   | S.Nexp_sum (left, right)   -> binary_operation (fun a b -> N.ExtendedType.IntExpression.Add (a, b)) left right
   | S.Nexp_minus (left, right) -> binary_operation (fun a b -> N.ExtendedType.IntExpression.Sub (a, b)) left right
   | S.Nexp_times (left, right) -> binary_operation (fun a b -> N.ExtendedType.IntExpression.Mul (a, b)) left right
   | S.Nexp_var id              -> begin
       let S.Kid_aux (Var unwrapped_id, _id_location) = id
       in
       TC.return @@ N.ExtendedType.IntExpression.Var unwrapped_id
     end


let extended_return_type_of_sail_type (sail_type : S.typ) : N.ExtendedType.ReturnValue.t TC.t =
  let S.Typ_aux (unwrapped_sail_type, sail_type_location) = sail_type
  in
  match unwrapped_sail_type with
   | S.Typ_internal_unknown -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_id _id           -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_var _            -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_fn (_, _)        -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_bidir (_, _)     -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_tuple _          -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_exist (_, _, _)  -> TC.not_yet_implemented [%here] sail_type_location
   | S.Typ_app (identifier, type_arguments) -> begin
       let Id_aux (unwrapped_identifier, identifier_location) = identifier
       in
       match unwrapped_identifier with
       | Id "atom" -> begin
           match type_arguments with
           | [ type_argument ] -> begin
               let S.A_aux (unwrapped_type_argument, type_argument_location) = type_argument
               in
               match unwrapped_type_argument with
                | S.A_typ _                   -> TC.not_yet_implemented [%here] type_argument_location
                | S.A_bool _                  -> TC.not_yet_implemented [%here] type_argument_location
                | S.A_nexp numeric_expression -> begin
                    let* int_expression = int_expression_of_sail_numeric_expression numeric_expression
                    in
                    TC.return @@ N.ExtendedType.ReturnValue.Int int_expression
                  end
             end
           | _ -> TC.not_yet_implemented ~message:"Unexpected number of type arguments (should be exactly one)" [%here] sail_type_location
         end
       | Id string -> begin
           let message =
             Printf.sprintf "Unknown type %s" string
           in
           TC.not_yet_implemented ~message [%here] identifier_location
         end
       | Operator _ -> TC.not_yet_implemented [%here] identifier_location
     end


(* Order not preserved! *)
let remove_string_duplicates (strings : string list) : string list =
  List.dedup_and_sort strings ~compare:String.compare


let collect_variable_names_in_parameter_type (parameter_type : N.ExtendedType.Parameter.t) : string list =
  let rec collect parameter_type =
    match parameter_type with
    | N.ExtendedType.Parameter.Tuple ts     -> List.concat @@ List.map ~f:collect ts
    | N.ExtendedType.Parameter.Int (Some k) -> [ k ]
    | N.ExtendedType.Parameter.Int None     -> [ ]
    | N.ExtendedType.Parameter.Bool k       -> [ k ]
    | N.ExtendedType.Parameter.Other _      -> []
  in
  remove_string_duplicates @@ collect parameter_type


(* Returned list can contain duplicates *)
let collect_variable_names_in_int_expression (int_expression : N.ExtendedType.IntExpression.t) : string list =
  let rec collect int_expression =
    match int_expression with
    | N.ExtendedType.IntExpression.Var k             -> [ k ]
    | N.ExtendedType.IntExpression.Constant _        -> []
    | N.ExtendedType.IntExpression.Add (left, right) -> collect left @ collect right
    | N.ExtendedType.IntExpression.Sub (left, right) -> collect left @ collect right
    | N.ExtendedType.IntExpression.Mul (left, right) -> collect left @ collect right
    | N.ExtendedType.IntExpression.Neg operand       -> collect operand
  in
  remove_string_duplicates @@ collect int_expression


let simple_name_from_index (index : int) : string =
  Printf.sprintf "#%d" index


let generate_simpler_names (names : string list) : string StringMap.t =
  let rec generate (index : int) (names : string list) (map : string StringMap.t) =
    match names with
    | []          -> map
    | name::names -> begin
        let simplified_name = simple_name_from_index index
        in
        let map' = StringMap.add_exn map ~key:name ~data:simplified_name
        in
        generate (index + 1) names map'
      end
  in
  let empty : string StringMap.t = StringMap.empty
  in
  generate 0 names empty


let substitute_in_parameter_type
    (map            : string StringMap.t        )
    (parameter_type : N.ExtendedType.Parameter.t) : N.ExtendedType.Parameter.t
  =
  let open N.ExtendedType.Parameter
  in
  let rec subst (parameter_type : t) =
    match parameter_type with
    | Tuple ts     -> Tuple (List.map ~f:subst ts)
    | Int (Some k) -> Int (Some (StringMap.find_exn map k))
    | Int None     -> Int None
    | Bool k       -> Bool (StringMap.find_exn map k)
    | Other _      -> parameter_type
  in
  subst parameter_type


let substitute_in_int_expression
    (map            : string StringMap.t            )
    (int_expression : N.ExtendedType.IntExpression.t) : N.ExtendedType.IntExpression.t
  =
  let open N.ExtendedType.IntExpression
  in
  let rec subst (int_expression : t) =
    match int_expression with
    | Var k             -> Var (StringMap.find_exn map k)
    | Constant _        -> int_expression
    | Add (left, right) -> Add (subst left, subst right)
    | Sub (left, right) -> Sub (subst left, subst right)
    | Mul (left, right) -> Mul (subst left, subst right)
    | Neg expr          -> Neg (subst expr)
  in
  subst int_expression


let substitute_in_return_type
    (map         : string StringMap.t          )
    (return_type : N.ExtendedType.ReturnValue.t) : N.ExtendedType.ReturnValue.t
  =
  match return_type with
  | N.ExtendedType.ReturnValue.Int int_expression -> N.ExtendedType.ReturnValue.Int (substitute_in_int_expression map int_expression)
  

(*
   Gives variables simpler names
*)
let simplify (extended_function_type : N.ExtendedFunctionType.t) : N.ExtendedFunctionType.t =
  let variable_names = remove_string_duplicates @@ List.concat @@ List.map ~f:collect_variable_names_in_parameter_type extended_function_type.extended_parameter_types
  in
  let variable_mapping = generate_simpler_names variable_names
  in
  let extended_parameter_types =
    List.map ~f:(substitute_in_parameter_type variable_mapping) extended_function_type.extended_parameter_types
  and extended_return_type =
    substitute_in_return_type variable_mapping extended_function_type.extended_return_type
  in
  {
    extended_parameter_types;
    extended_return_type
  }
  

let determine_extended_type
      (parameter_bindings : N.type_annotation Libsail.Ast.pat)
      (return_type        : Libsail.Ast.typ                  ) : N.ExtendedFunctionType.t TC.t
  =
  let* parameter_types = unpack_parameter_types parameter_bindings
  in
  let* extended_parameter_types = TC.map ~f:extended_parameter_type_of_sail_type parameter_types
  and* extended_return_type     = extended_return_type_of_sail_type return_type
  in
  let extended_function_type : N.ExtendedFunctionType.t =
    { extended_parameter_types; extended_return_type }
  in
  TC.return @@ simplify extended_function_type
