open! ExtBase
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters

open Shared


let define_macro =
  let id = "define-macro"

  and define_macro (args : Value.t list) =
    match args with
    | form :: body -> begin
        let=? function_name, parameters = C.(cons symbol (list symbol)) form
        in
        let* env = EC.(get State.environment)
        in
        let  callable = Value.Callable (Evaluation.mk_macro env parameters body)
        in
        let* () = EC.add_binding function_name callable
        in
        EC.return @@ Some (Value.Nil)
      end
    | _ -> EC.return None

  in
  (id, Functions.mk_multi_special_form [ define_macro; error id ])


let initialize =
  let definitions = [
    define_macro;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
