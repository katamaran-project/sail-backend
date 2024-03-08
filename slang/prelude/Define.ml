open Base
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext
module C = Converters

open Shared


let define =
  let id = "define"

  and define_function (args : Value.t list) =
    match args with
    | form :: body -> begin
        let=? function_name, parameters = C.(cons symbol (list symbol)) form
        in
        let* env      = EC.current_environment
        in
        let  callable = Value.Callable (Evaluation.mk_closure env parameters body)
        in
        let* ()       = EC.add_binding function_name callable
        in
        EC.return @@ Some (Value.Nil)
      end
    | _ -> EC.return None

  and define_variable (args : Value.t list) =
    let=? identifier, expression = C.(map2 symbol value) args
    in
    let* value = Evaluation.evaluate expression
    in
    let* () = EC.add_binding identifier value
    in
    EC.return @@ Some (Value.Nil)

  in
  (id, Functions.mk_multi_special_form [ define_function; define_variable; error id ])


let initialize =
  let definitions = [
    define;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Auxlib.uncurry EC.add_binding) pairs
