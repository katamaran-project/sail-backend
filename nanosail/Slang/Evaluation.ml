module P = Parser
open! ExtBase
open Exception


module EC = EvaluationContext
open Monads.Notations.Star(EC)


let bind_parameters parameters arguments =
  match List.zip parameters arguments with
  | List.Or_unequal_lengths.Ok pairs        -> EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
  | List.Or_unequal_lengths.Unequal_lengths -> raise @@ SlangError "wrong number of parameters"


let with_environment
    (env  : Value.t Environment.t)
    (func : 'a EC.t              ) : 'a EC.t
  =
  let* old_env = EC.(get environment)
  in
  let* ()      = EC.(put environment) env
  in
  let* result  = func
  in
  let* ()      = EC.(put environment) old_env
  in
  EC.return result


let rec evaluate (ast : Value.t) : Value.t EC.t =
  match ast with
  | Value.Cons (id, args)   -> begin
      let* f = evaluate id
      in
      match Value.cons_to_list args with
      | Some vs -> evaluate_call f vs
      | _       -> raise @@ SlangError "invalid call: expected well-formed list"
    end
  | Value.Symbol identifier -> begin
      if
        Value.is_keyword identifier
      then
        (* keywords evaluate to themselves *)
        EC.return ast
      else begin
        let* lookup_result = EC.lookup identifier
        in
        match lookup_result with
        | Some value          -> EC.return value
        | None                -> raise @@ SlangError ("unbound identifier " ^ identifier)
      end
    end
  | Value.Integer _      -> EC.return ast
  | Value.String _       -> EC.return ast
  | Value.Bool _         -> EC.return ast
  | Value.Nil            -> EC.return ast
  | Value.Callable _     -> EC.return ast
  | Value.Reference _    -> EC.return ast


and evaluate_call func arguments =
  match func with
  | Value.Cons (_, _)    -> raise @@ SlangError "cons are not callable"
  | Value.Integer _      -> raise @@ SlangError "integers are not callable"
  | Value.Symbol _       -> raise @@ SlangError "symbols are not callable"
  | Value.String _       -> raise @@ SlangError "strings are not callable"
  | Value.Bool _         -> raise @@ SlangError "bools are not callable"
  | Value.Nil            -> raise @@ SlangError "nil is not callable"
  | Value.Reference _    -> raise @@ SlangError "references are not callable"
  | Value.Callable f     -> evaluate_callable f arguments


and evaluate_callable native_function arguments =
  native_function arguments


(*
   Evaluates each value in order and returns the results in a list.
*)
let evaluate_sequentially (asts : Value.t list) : Value.t list EC.t =
  EC.map ~f:evaluate asts


(*
   Evaluates each value in order.
   Returns the result of the evaluation of the last value (or Nil if the given list is empty).
*)
let evaluate_block (block : Value.t list) : Value.t EC.t =
  let* results = evaluate_sequentially block
  in
  EC.return @@ Option.value ~default:Value.Nil @@ List.last results


let mk_closure
    (env        : Value.t Environment.t)
    (parameters : string list          )
    (body       : Value.t list         ) : Value.callable
  =
  let rec callable arguments =
    let* evaluated_arguments = EC.map ~f:evaluate arguments
    in
    with_environment env begin
      let* () = bind_parameters parameters evaluated_arguments
      in
      let* () = EC.add_binding "recurse" (Value.Callable callable)
      in
      let* values =
        evaluate_block body
      in
      EC.return values
    end
  in
  callable


let mk_macro env parameters body : Value.callable =
  let rec callable arguments =
    with_environment env begin
      let* ()     = bind_parameters parameters arguments
      in
      let* ()     = EC.add_binding "recurse" (Value.Callable callable)
      in
      let* result = evaluate_block body
      in
      evaluate result
    end
  in
  callable


let parse_and_evaluate_string (s : string) : Value.t EC.t =
  let asts = P.parse_string s
  in
  evaluate_block asts
