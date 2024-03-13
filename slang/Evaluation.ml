module P = Parsing
open Base
open Auxlib
open Exception


module EC = struct
  include EvaluationContext
  include Monads.Notations.Star(EvaluationContext)
end


let bind_parameters parameters arguments =
  match List.zip parameters arguments with
  | List.Or_unequal_lengths.Ok pairs        -> EC.iter ~f:(uncurry EC.add_binding) pairs
  | List.Or_unequal_lengths.Unequal_lengths -> raise @@ SlangError "wrong number of parameters"


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
      | _       -> raise @@ SlangError "invalid call: expected well-formed list"
    end
  | Value.Symbol identifier -> begin
      let* lookup_result = lookup identifier
      in
      match lookup_result with
      | Some value          -> return value
      | None                -> raise @@ SlangError ("unbound identifier " ^ identifier)
    end
  | Value.Integer _      -> return ast
  | Value.String _       -> return ast
  | Value.Bool _         -> return ast
  | Value.Nil            -> return ast
  | Value.Callable _     -> return ast

and evaluate_call func arguments =
  match func with
  | Value.Cons (_, _)    -> raise @@ SlangError "cons are not callable"
  | Value.Integer _      -> raise @@ SlangError "integers are not callable"
  | Value.Symbol _       -> raise @@ SlangError "symbols are not callable"
  | Value.String _       -> raise @@ SlangError "strings are not callable"
  | Value.Bool _         -> raise @@ SlangError "bools are not callable"
  | Value.Nil            -> raise @@ SlangError "nil is not callable"
  | Value.Callable f     -> evaluate_callable f arguments

and evaluate_callable native_function arguments =
  native_function arguments

and evaluate_many asts =
  let open EC
  in
  let* results = EC.map ~f:evaluate asts
  in
  match List.last results with
  | None   -> return Value.Nil
  | Some x -> return x

let mk_closure environment parameters body : Value.callable =
  let rec callable arguments =
    let open EC
    in
    let* evaluated_arguments = map ~f:evaluate arguments
    in
    with_environment environment begin
      let* () = bind_parameters parameters evaluated_arguments
      in
      let* () = add_binding "recurse" (Value.Callable callable)
      in
      evaluate_many body
    end
  in
  callable

let mk_macro environment parameters body : Value.callable =
  let rec callable arguments =
    let open EC
    in
    with_environment environment begin
      let* () = bind_parameters parameters arguments
      in
      let* () = add_binding "recurse" (Value.Callable callable)
      in
      let* result = evaluate_many body
      in
      evaluate result
    end
  in
  callable

let evaluate_string (s : string) : Value.t EC.t =
  let asts = P.parse_string s
  in
  evaluate_many asts
