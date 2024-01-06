open Base
open Auxlib

module EvaluationContext = Evaluation_context




let bind_parameters parameters arguments =
  match List.zip parameters arguments with
  | List.Or_unequal_lengths.Ok pairs        -> EvaluationContext.iter (uncurry EvaluationContext.bind) pairs
  | List.Or_unequal_lengths.Unequal_lengths -> raise @@ EvaluationError "wrong number of parameters"

let with_environment env func =
  let open EvaluationContext
  in
  let* old_env = current_environment in
  let* ()      = set_current_environment env in
  let* result  = func () in
  let* ()      = set_current_environment old_env
  in
  return result

let rec evaluate (ast : Value.t) =
  let open EvaluationContext
  in
  match ast with
  | Value.Cons (id, args)   -> let* f = evaluate id in evaluate_call f @@ Value.cons_to_list args
  | Value.Symbol identifier -> lookup identifier
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
  let open EvaluationContext
  in
  let* evaluated_arguments =
    map evaluate arguments
  in
  with_environment environment (fun () ->
      let* () = bind_parameters parameters evaluated_arguments
      in
      evaluate_many body
    )

and evaluate_native_call native_function arguments =
  let open EvaluationContext
  in
  let* env = current_environment
  in
  EvaluationContext.return @@ native_function env arguments

and evaluate_many asts =
  let open EvaluationContext
  in
  let* results = EvaluationContext.map evaluate asts
  in
  match List.last results with
  | None   -> return Value.Nil
  | Some x -> return x


let run asts =
  EvaluationContext.run (evaluate_many asts) Prelude.prelude
