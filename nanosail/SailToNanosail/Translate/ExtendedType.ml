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


module ExtendedType = struct
  type t =
    | Tuple of t list
    | Int   of string
    | Bool  of string
    | Other of string

  let rec string_of (extended_type : t) : string =
    match extended_type with
    | Tuple ts -> String.concat ~sep:" * " @@ List.map ~f:(fun t -> Printf.sprintf "(%s)" (string_of t)) ts
    | Int k    -> Printf.sprintf "Int(#%s)" k
    | Bool k   -> Printf.sprintf "Bool(#%s)" k
    | Other s  -> s
end


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
let extended_parameter_type_of_sail_type (sail_type : S.typ) : ExtendedType.t TC.t =
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
                         TC.return @@ ExtendedType.Int unwrapped_kid
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


let determine_extended_type
      (parameter_bindings : N.type_annotation Libsail.Ast.pat)
      (_return_type        : Libsail.Ast.typ                  )
  =
  let* parameter_types = unpack_parameter_types parameter_bindings
  in
  let* _extended_parameter_types = TC.map ~f:extended_parameter_type_of_sail_type parameter_types
  in
  TC.return ()
