module Big_int = Nat_big_num

module S = struct
  include Libsail
  include Libsail.Ast
  include Libsail.Ast_defs
  include Libsail.Ast_util
  include Libsail.Anf
end

module TC = TranslationContext
module StringMap = Map.String

open Base (* Must come after module StringMap = Map.String because of conflicting module names *)
open Monads.Notations.Star(TC)


module State = struct
  type mapping = int StringMap.t

  type t =
    {
      next_id : int;
      mapping : mapping
    }

  let initial : t =
    {
      next_id = 0;
      mapping = StringMap.empty;
    }

  let next_id =
    let get (state : t)                 : int = state.next_id
    and set (state : t) (next_id : int) : t   = { state with next_id }
    in
    (get, set)

  let mapping =
    let get (state : t)                     : mapping = state.mapping
    and set (state : t) (mapping : mapping) : t       = { state with mapping }
    in
    (get, set)
end


module Error = struct
  type t =
    | NotYetImplemented of Lexing.position * Libsail.Ast.l * string option
    | Failure of Lexing.position * string
end


module Monad = Monads.StateResult.Make(State)(Error)
open Monads.Notations.Plus(Monad)


module MonadUtil = Monads.Util.Make(Monad)
include MonadUtil


let next_id : int Monad.t =
  let+ id = Monad.get State.next_id
  in
  let+ () = Monad.put State.next_id (id + 1)
  in
  Monad.return id


let add_mapping key data : unit Monad.t =
  let+ mapping = Monad.get State.mapping
  in
  let+ () = Monad.put State.mapping @@ StringMap.add_exn mapping ~key ~data
  in
  Monad.return ()


let lookup key : int option Monad.t =
  let+ mapping = Monad.get State.mapping
  in
  Monad.return @@ StringMap.find mapping key


(* Forces a fresh binding; error if s is already bound *)
let fresh_binding (s : string) : int Monad.t =
  let+ id = next_id
  in
  let+ () = add_mapping s id
  in
  Monad.return id


(* Returns old binding if it exists, creates new one if necessary *)
let binding (s : string) : int Monad.t =
  let+ id = lookup s
  in
  match id with
  | None    -> fresh_binding s
  | Some id -> Monad.return id


let not_yet_implemented ?(message = "") ocaml_position sail_position =
  let message =
    if String.is_empty message
    then None
    else Some message
  in
  Monad.fail @@ NotYetImplemented (ocaml_position, sail_position, message)


let fail ocaml_position message =
  Monad.fail @@ Failure (ocaml_position, message)


(*
  Returns the parameter types as a list of sail types
*)
let unpack_parameter_types (parameter_bindings : Sail.type_annotation Libsail.Ast.pat) : S.typ list Monad.t =
  let P_aux (_unwrapped_parameter_bindings, parameter_bindings_annotation) = parameter_bindings
  in
  let parameter_bundle_type = Libsail.Type_check.typ_of_annot parameter_bindings_annotation
  in
  let Typ_aux (unwrapped_parameter_bundle_type, _parameter_bundle_type_location) = parameter_bundle_type
  in
  match unwrapped_parameter_bundle_type with
  | Typ_tuple ts -> Monad.return ts
  | _            -> Monad.return [ parameter_bundle_type ]


(*
  Focuses on parameter types.
  Atoms must only contain a single identifier as their type argument.
*)
let extended_parameter_type_of_sail_type (sail_type : S.typ) : Ast.ExtendedType.Parameter.t Monad.t =
  let Typ_aux (unwrapped_sail_type, sail_type_location) = sail_type
  in
  let extended_parameter_type_of_atom (type_arguments : S.typ_arg list) =
    match type_arguments with
    | [ type_argument ] -> begin
        let A_aux (unwrapped_type_argument, type_argument_location) = type_argument
        in
        match unwrapped_type_argument with
        | A_typ typ                   -> begin
            Stdio.printf "%s\n" (StringOf.Sail.typ typ);
            not_yet_implemented [%here] type_argument_location
          end
        | A_bool _                    -> not_yet_implemented [%here] type_argument_location
        | A_nexp numerical_expression -> begin
            let Nexp_aux (unwrapped_numerical_expression, numerical_expression_location) = numerical_expression
            in
            match unwrapped_numerical_expression with
            | Nexp_id _         -> not_yet_implemented [%here] numerical_expression_location
            | Nexp_constant _   -> not_yet_implemented [%here] numerical_expression_location
            | Nexp_app (_, _)   -> not_yet_implemented [%here] numerical_expression_location
            | Nexp_times (_, _) -> not_yet_implemented [%here] numerical_expression_location
            | Nexp_sum (_, _)   -> not_yet_implemented [%here] numerical_expression_location
            | Nexp_minus (_, _) -> not_yet_implemented [%here] numerical_expression_location
            | Nexp_exp _        -> not_yet_implemented [%here] numerical_expression_location
            | Nexp_neg _        -> not_yet_implemented [%here] numerical_expression_location
            | Nexp_var kid      -> begin
                let Kid_aux (Var unwrapped_kid, _kid_location) = kid
                in
                let+ translated_id = fresh_binding unwrapped_kid
                in
                Monad.return @@ Ast.ExtendedType.Parameter.Int translated_id
              end
          end
      end
    | _ -> not_yet_implemented ~message:"Unexpected number of type arguments (should be exactly one)" [%here] sail_type_location

  and extended_parameter_type_of_atom_bool (type_arguments : S.typ_arg list) =
    match type_arguments with
    | [ type_argument ] -> begin
        let A_aux (unwrapped_type_argument, type_argument_location) = type_argument
        in
        match unwrapped_type_argument with
        | A_typ _                     -> not_yet_implemented [%here] type_argument_location
        | A_nexp _                    -> not_yet_implemented [%here] type_argument_location
        | A_bool numeric_constraint   -> begin
            let S.NC_aux (unwrapped_numeric_constraint, numeric_constraint_location) = numeric_constraint
            in
            match unwrapped_numeric_constraint with
            | S.NC_equal (_, _)      -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_bounded_ge (_, _) -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_bounded_gt (_, _) -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_bounded_le (_, _) -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_bounded_lt (_, _) -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_not_equal (_, _)  -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_set (_, _)        -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_or (_, _)         -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_and (_, _)        -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_app (_, _)        -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_true              -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_false             -> not_yet_implemented [%here] numeric_constraint_location
            | S.NC_var kid           -> begin
                let Kid_aux (Var unwrapped_kid, _kid_location) = kid
                in
                let+ translated_id = fresh_binding unwrapped_kid
                in
                Monad.return @@ Ast.ExtendedType.Parameter.Bool translated_id
              end
          end
      end
    | _ -> not_yet_implemented ~message:"Unexpected number of type arguments (should be exactly one)" [%here] sail_type_location

  and extended_parameter_type_of_bitvector
      (location       : Libsail.Ast.l )
      (type_arguments : S.typ_arg list) :  Ast.ExtendedType.Parameter.t Monad.t
    =
    match type_arguments with
    | [ _ ] -> begin
        Monad.return @@ Ast.ExtendedType.Parameter.Unknown {
          ocaml_location = [%here];
          sail_location = location;
          annotation = "List not yet supported"
        }
      end
    | _ -> fail [%here] "list should have only one type argument"
  in

  match unwrapped_sail_type with
   | Typ_internal_unknown -> not_yet_implemented [%here] sail_type_location
   | Typ_var _            -> not_yet_implemented [%here] sail_type_location
   | Typ_fn (_, _)        -> not_yet_implemented [%here] sail_type_location
   | Typ_bidir (_, _)     -> not_yet_implemented [%here] sail_type_location
   | Typ_tuple _          -> not_yet_implemented [%here] sail_type_location
   | Typ_exist (_, _, _)  -> not_yet_implemented [%here] sail_type_location
   | Typ_id id            -> Monad.return @@ Ast.ExtendedType.Parameter.Other (StringOf.Sail.id id)
   | Typ_app (identifier, type_arguments) -> begin
       let Id_aux (unwrapped_identifier, location) = identifier
       in
       match unwrapped_identifier with
       | Id "atom"      -> extended_parameter_type_of_atom type_arguments
       | Id "atom_bool" -> extended_parameter_type_of_atom_bool type_arguments
       | Id "bitvector" -> extended_parameter_type_of_bitvector location type_arguments
       | Id string      -> begin
           let message =
             Printf.sprintf "Unknown type %s" string
           in
           Monad.return @@ Ast.ExtendedType.Parameter.Unknown {
             ocaml_location = [%here];
             sail_location = location;
             annotation = message
           }
         end
       | Operator _ -> not_yet_implemented [%here] location
     end


let rec int_expression_of_sail_numeric_expression (numeric_expression : S.nexp) : Ast.ExtendedType.IntExpression.t Monad.t =
  let binary_operation
        (factory : Ast.ExtendedType.IntExpression.t -> Ast.ExtendedType.IntExpression.t -> Ast.ExtendedType.IntExpression.t)
        (left    : S.nexp                                                                                                  )
        (right   : S.nexp                                                                                                  ) : Ast.ExtendedType.IntExpression.t Monad.t
    =
    let+ left'  = int_expression_of_sail_numeric_expression left
    and+ right' = int_expression_of_sail_numeric_expression right
    in
    Monad.return @@ factory left' right'
  in
  let Nexp_aux (unwrapped_numeric_expression, numeric_expression_location) = numeric_expression
  in
  match unwrapped_numeric_expression with
   | Nexp_id _                -> not_yet_implemented [%here] numeric_expression_location
   | Nexp_app (_, _)          -> not_yet_implemented [%here] numeric_expression_location
   | Nexp_exp _               -> not_yet_implemented [%here] numeric_expression_location
   | Nexp_neg _               -> not_yet_implemented [%here] numeric_expression_location
   | Nexp_constant n          -> Monad.return @@ Ast.ExtendedType.IntExpression.Constant n
   | Nexp_sum (left, right)   -> binary_operation (fun a b -> Ast.ExtendedType.IntExpression.Add (a, b)) left right
   | Nexp_minus (left, right) -> binary_operation (fun a b -> Ast.ExtendedType.IntExpression.Sub (a, b)) left right
   | Nexp_times (left, right) -> binary_operation (fun a b -> Ast.ExtendedType.IntExpression.Mul (a, b)) left right
   | Nexp_var kid             -> begin
       let Kid_aux (Var unwrapped_id, _id_location) = kid
       in
       let+ translated_id = binding unwrapped_id
       in
       Monad.return @@ Ast.ExtendedType.IntExpression.Var translated_id
     end

and bool_expression_of_sail_numeric_constraint (numeric_constraint : S.n_constraint) : Ast.ExtendedType.BoolExpression.t Monad.t =
  let bool_expression_of_binary_operation
        (factory : Ast.ExtendedType.BoolExpression.t -> Ast.ExtendedType.BoolExpression.t -> Ast.ExtendedType.BoolExpression.t)
        (left    : S.n_constraint                                                                                             )
        (right   : S.n_constraint                                                                                             ) : Ast.ExtendedType.BoolExpression.t Monad.t
    =
    let+ left'  = bool_expression_of_sail_numeric_constraint left
    and+ right' = bool_expression_of_sail_numeric_constraint right
    in
    Monad.return @@ factory left' right'

  and bool_expression_of_comparison
        (factory : Ast.ExtendedType.IntExpression.t -> Ast.ExtendedType.IntExpression.t -> Ast.ExtendedType.BoolExpression.t)
        (left    : S.nexp                                                                                                   )
        (right   : S.nexp                                                                                                   ) : Ast.ExtendedType.BoolExpression.t Monad.t
    =
      let+ left'  = int_expression_of_sail_numeric_expression left
      and+ right' = int_expression_of_sail_numeric_expression right
      in
      Monad.return @@ factory left' right'
  in

  let bool_expression_of_and        = bool_expression_of_binary_operation @@ fun a b -> Ast.ExtendedType.BoolExpression.And (a, b)
  and bool_expression_of_or         = bool_expression_of_binary_operation @@ fun a b -> Ast.ExtendedType.BoolExpression.Or (a, b)
  and bool_expression_of_equal      = bool_expression_of_comparison       @@ fun a b -> Ast.ExtendedType.BoolExpression.Equal (a, b)
  and bool_expression_of_not_equal  = bool_expression_of_comparison       @@ fun a b -> Ast.ExtendedType.BoolExpression.NotEqual (a, b)
  and bool_expression_of_bounded_lt = bool_expression_of_comparison       @@ fun a b -> Ast.ExtendedType.BoolExpression.LessThan (a, b)
  and bool_expression_of_bounded_le = bool_expression_of_comparison       @@ fun a b -> Ast.ExtendedType.BoolExpression.LessThanOrEqualTo (a, b)
  and bool_expression_of_bounded_gt = bool_expression_of_comparison       @@ fun a b -> Ast.ExtendedType.BoolExpression.GreaterThan (a, b)
  and bool_expression_of_bounded_ge = bool_expression_of_comparison       @@ fun a b -> Ast.ExtendedType.BoolExpression.GreaterThanOrEqualTo (a, b)
  in

  let NC_aux (unwrapped_numeric_constraint, numeric_constraint_location) = numeric_constraint
  in
  match unwrapped_numeric_constraint with
  | NC_bounded_lt (left, right) -> bool_expression_of_bounded_lt left right
  | NC_bounded_le (left, right) -> bool_expression_of_bounded_le left right
  | NC_bounded_gt (left, right) -> bool_expression_of_bounded_gt left right
  | NC_bounded_ge (left, right) -> bool_expression_of_bounded_ge left right
  | NC_set (_, _)               -> not_yet_implemented [%here] numeric_constraint_location
  | NC_app (_, _)               -> not_yet_implemented [%here] numeric_constraint_location
  | NC_true                     -> not_yet_implemented [%here] numeric_constraint_location
  | NC_false                    -> not_yet_implemented [%here] numeric_constraint_location
  | NC_and (left, right)        -> bool_expression_of_and left right
  | NC_or  (left, right)        -> bool_expression_of_or left right
  | NC_equal (left, right)      -> bool_expression_of_equal left right
  | NC_not_equal (left, right)  -> bool_expression_of_not_equal left right
  | NC_var kid           -> begin
       let Kid_aux (Var unwrapped_id, _id_location) = kid
       in
       let+ translated_id = binding unwrapped_id
       in
       Monad.return @@ Ast.ExtendedType.BoolExpression.Var translated_id
    end


let rec extended_return_type_of_sail_type (sail_type : S.typ) : Ast.ExtendedType.ReturnValue.t Monad.t =
  let Typ_aux (unwrapped_sail_type, sail_type_location) = sail_type
  in

  let extended_return_type_of_atom (type_arguments : S.typ_arg list) : Ast.ExtendedType.ReturnValue.t Monad.t =
    match type_arguments with
    | [ type_argument ] -> begin
        let A_aux (unwrapped_type_argument, location) = type_argument
        in
        match unwrapped_type_argument with
        | A_typ _                   -> not_yet_implemented [%here] location
        | A_bool _                  -> not_yet_implemented [%here] location
        | A_nexp numeric_expression -> begin
            let+ int_expression = int_expression_of_sail_numeric_expression numeric_expression
            in
            Monad.return @@ Ast.ExtendedType.ReturnValue.Int int_expression
          end
      end
    | _ -> not_yet_implemented ~message:"Unexpected number of type arguments (should be exactly one)" [%here] sail_type_location
  in

  let extended_return_type_of_atom_bool (type_arguments : S.typ_arg list) : Ast.ExtendedType.ReturnValue.t Monad.t =
    match type_arguments with
    | [ type_argument ] -> begin
        let A_aux (unwrapped_type_argument, type_argument_location) = type_argument
        in
        match unwrapped_type_argument with
        | A_typ _                   -> not_yet_implemented [%here] type_argument_location
        | A_nexp _                  -> not_yet_implemented [%here] type_argument_location
        | A_bool numeric_constraint -> begin
            let+ bool_expression = bool_expression_of_sail_numeric_constraint numeric_constraint
            in
            Monad.return @@ Ast.ExtendedType.ReturnValue.Bool bool_expression
          end
      end
    | _ -> fail [%here] "Unexpected number of type arguments (should be exactly one)"
  in

  let extended_return_type_of_list
      (location       : Libsail.Ast.l )
      (type_arguments : S.typ_arg list) : Ast.ExtendedType.ReturnValue.t Monad.t
    =
    match type_arguments with
    | [ _ ] -> begin
        Monad.return @@ Ast.ExtendedType.ReturnValue.Unknown {
          ocaml_location = [%here];
          sail_location = location;
          annotation = "List not yet supported"
        }
      end
    | _ -> fail [%here] "list should have only one type argument"
  in

  let extended_return_type_of_bitvector
      (location       : Libsail.Ast.l)
      (type_arguments : S.typ_arg list) : Ast.ExtendedType.ReturnValue.t Monad.t
    =
    match type_arguments with
    | [ _ ] -> begin
        Monad.return @@ Ast.ExtendedType.ReturnValue.Unknown {
          ocaml_location = [%here];
          sail_location = location;
          annotation = "Bitvectors not yet supported"
        }
      end
    | _ -> fail [%here] "bitvectors expected to have only one type argument"
  in

  match unwrapped_sail_type with
   | Typ_internal_unknown -> not_yet_implemented [%here] sail_type_location
   | Typ_var _            -> not_yet_implemented [%here] sail_type_location
   | Typ_fn (_, _)        -> not_yet_implemented [%here] sail_type_location
   | Typ_bidir (_, _)     -> not_yet_implemented [%here] sail_type_location
   | Typ_tuple ts         -> begin
       let+ ts' = map ~f:extended_return_type_of_sail_type ts
       in
       let tuple = Ast.ExtendedType.ReturnValue.Tuple ts'
       in
       Monad.return tuple
     end
   | Typ_exist (_, _, _)  -> begin
       Monad.return @@ Ast.ExtendedType.ReturnValue.Unknown {
         ocaml_location = [%here];
         sail_location  = sail_type_location;
         annotation     = StringOf.Sail.typ sail_type
       }
     end
   | Typ_id id -> begin
       let Id_aux (unwrapped_id, id_location) = id
       in
       match unwrapped_id with
       | S.Operator _ -> not_yet_implemented [%here] id_location
       | S.Id name    -> begin
           match name with
           | "int"  -> let+ k = next_id in Monad.return @@ Ast.ExtendedType.ReturnValue.Int (Ast.ExtendedType.IntExpression.Var k)
           | "bool" -> let+ k = next_id in Monad.return @@ Ast.ExtendedType.ReturnValue.Bool (Ast.ExtendedType.BoolExpression.Var k)
           | id     -> Monad.return @@ Ast.ExtendedType.ReturnValue.Other id
         end
     end
   | Typ_app (identifier, type_arguments) -> begin
       let Id_aux (unwrapped_identifier, location) = identifier
       in
       match unwrapped_identifier with
       | Id "atom"      -> extended_return_type_of_atom type_arguments
       | Id "atom_bool" -> extended_return_type_of_atom_bool type_arguments
       | Id "list"      -> extended_return_type_of_list location type_arguments
       | Id "bitvector" -> extended_return_type_of_bitvector location type_arguments
       | Id string -> begin
           let message =
             Printf.sprintf "Unknown type %s" string
           in
           not_yet_implemented ~message [%here] location
         end
       | Operator _ -> not_yet_implemented [%here] location
     end


(* Order not preserved! *)
let remove_string_duplicates (strings : string list) : string list =
  List.dedup_and_sort strings ~compare:String.compare


let determine_extended_type
      (parameter_bindings : Sail.type_annotation Libsail.Ast.pat)
      (return_type        : Libsail.Ast.typ                     ) : Ast.ExtendedFunctionType.t TC.t
  =
  let monad =
    let+ parameter_types = unpack_parameter_types parameter_bindings
    in
    let+ extended_parameter_types = map ~f:extended_parameter_type_of_sail_type parameter_types
    and+ extended_return_type     = extended_return_type_of_sail_type return_type
    in
    let extended_function_type : Ast.ExtendedFunctionType.t =
      { extended_parameter_types; extended_return_type }
    in
    Monad.return extended_function_type
  in
  let (result, _final_state) = Monad.run monad State.initial
  in
  match result with
  | Monad.Success t                                                                    -> TC.return t
  | Monad.Failure (Error.NotYetImplemented (ocaml_location, source_location, message)) -> begin
      match message with
      | Some message -> TC.not_yet_implemented ~message:message ocaml_location source_location
      | None         -> TC.not_yet_implemented ocaml_location source_location
    end
  | Monad.Failure (Error.Failure (ocaml_location, message)) ->
      TC.fail ocaml_location message
