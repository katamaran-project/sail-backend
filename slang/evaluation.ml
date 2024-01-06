open Base
open Auxlib


module EC = struct
  include Evaluation_context
  include Monads.Notations.Star(Evaluation_context)
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
  | Value.Cons (id, args)   -> let* f = evaluate id in evaluate_call f @@ Value.cons_to_list args
  | Value.Symbol identifier -> begin
      let* lookup_result = lookup identifier
      in
      match lookup_result with
      | Some value          -> return value
      | None                -> raise @@ EvaluationError ("unbound identifier " ^ identifier)
    end
  | Value.Integer _         -> return ast
  | Value.String _          -> return ast
  | Value.Bool _            -> return ast
  | Value.Nil               -> return ast
  | Value.NativeFunction _  -> return ast
  | Value.Closure _         -> return ast

and evaluate_call func arguments =
  match func with
  | Value.Cons (_, _)                 -> raise @@ EvaluationError "cons are not callable"
  | Value.Integer _                   -> raise @@ EvaluationError "integers are not callable"
  | Value.Symbol _                    -> raise @@ EvaluationError "symbols are not callable"
  | Value.String _                    -> raise @@ EvaluationError "strings are not callable"
  | Value.Bool _                      -> raise @@ EvaluationError "bools are not callable"
  | Value.Nil                         -> raise @@ EvaluationError "nil is not callable"
  | Value.Closure (env, params, body) -> evaluate_closure_call env params arguments body
  | Value.NativeFunction f            -> evaluate_native_call f arguments

and evaluate_closure_call environment parameters arguments body =
  let open EC
  in
  let* evaluated_arguments =
    map evaluate arguments
  in
  with_environment environment begin
    let* () = bind_parameters parameters evaluated_arguments
    in
    evaluate_many body
  end

and evaluate_native_call native_function arguments =
  native_function arguments

and evaluate_many asts =
  let open EC
  in
  let* results = EC.map evaluate asts
  in
  match List.last results with
  | None   -> return Value.Nil
  | Some x -> return x
