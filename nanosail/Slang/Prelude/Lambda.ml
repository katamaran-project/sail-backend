open! ExtBase
open Exception
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters

open Shared


let lambda =
  let id = "lambda"
  in
  let impl args =
    match args with
    | []             -> raise @@ SlangError "ill-formed lambda"
    | [_]            -> raise @@ SlangError "ill-formed lambda"
    | params :: body -> begin
        let*   env    = EC.(get environment)   in
        let=!  params = C.list C.symbol params in
        let=!! body   = List.map ~f:C.value body
        in
        EC.return @@ Value.Callable (Evaluation.mk_closure env params body)
      end
  in
  (id, impl)


let initialize =
  let definitions = [
    lambda;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
