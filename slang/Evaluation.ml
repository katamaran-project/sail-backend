open Base
open Auxlib


module EC = struct
  include EvaluationContext
  include Monads.Notations.Star(EvaluationContext)
end

exception EvaluationError of string

let bind_parameters parameters arguments =
  match List.zip parameters arguments with
  | List.Or_unequal_lengths.Ok pairs        -> EC.iter (uncurry EC.add_binding) pairs
  | List.Or_unequal_lengths.Unequal_lengths -> raise @@ EvaluationError "wrong number of parameters"

let with_environment (env : Value.t Environment.t) (func : 'a EC.t) : 'a EC.t =
  let open EC
  in
  let* old_env = current_environment in
  let* ()      = set_current_environment env in
  let* result  = func in
  let* ()      = set_current_environment old_env
  in
  return result


let rec evaluate (ast : Value.t) : Value.t EC.t =
  let open EC
  in
  match ast with
  | Value.Cons (id, args)   -> begin
      let* f = evaluate id
      in
      match Value.cons_to_list args with
      | Some vs -> evaluate_call f vs
      | _       -> raise @@ EvaluationError "invalid call: expected well-formed list"
    end
  | Value.Symbol identifier -> begin
      let* lookup_result = lookup identifier
      in
      match lookup_result with
      | Some value          -> return value
      | None                -> raise @@ EvaluationError ("unbound identifier " ^ identifier)
    end
  | Value.Integer _      -> return ast
  | Value.String _       -> return ast
  | Value.Bool _         -> return ast
  | Value.Nil            -> return ast
  | Value.Callable _     -> return ast

and evaluate_call func arguments =
  match func with
  | Value.Cons (_, _)    -> raise @@ EvaluationError "cons are not callable"
  | Value.Integer _      -> raise @@ EvaluationError "integers are not callable"
  | Value.Symbol _       -> raise @@ EvaluationError "symbols are not callable"
  | Value.String _       -> raise @@ EvaluationError "strings are not callable"
  | Value.Bool _         -> raise @@ EvaluationError "bools are not callable"
  | Value.Nil            -> raise @@ EvaluationError "nil is not callable"
  | Value.Callable f     -> evaluate_callable f arguments

and evaluate_callable native_function arguments =
  native_function arguments

and evaluate_many asts =
  let open EC
  in
  let* results = EC.map evaluate asts
  in
  match List.last results with
  | None   -> return Value.Nil
  | Some x -> return x

let mk_closure environment parameters body : Value.callable =
  fun arguments -> begin
      let open EC
      in
      let* evaluated_arguments = map evaluate arguments
      in
      with_environment environment begin
        let* () = bind_parameters parameters evaluated_arguments
        in
        evaluate_many body
      end
    end
