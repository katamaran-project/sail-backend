open Base
open ExtBase
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
        let* env      = EC.(get environment)
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


let destructuring_bind =
  let id = "destructuring-bind"

  and destructuring_bind (args : Value.t list) =
    match args with
    | pattern :: values :: body -> begin
        let patterns = match Value.cons_to_list pattern with
          | Some patterns -> patterns
          | None          -> raise @@ Exception.SlangError "invalid use of destructuring-bind"
        in
        let* values =
          let* evaluated_values = Evaluation.evaluate values
          in
          match Value.cons_to_list evaluated_values with
          | Some values -> EC.return values
          | None        -> raise @@ Exception.SlangError "invalid use of destructuring-bind"
        in
        let* () = Destructuring.destructure patterns values
        in
        let* evaluation_results = EC.map ~f:Evaluation.evaluate body
        in
        match List.last evaluation_results with
        | Some x -> EC.return x
        | None   -> EC.return Value.Nil
      end
    | _ -> raise @@ Exception.SlangError "invalid use of destructuring-bind"
  in

  (id, destructuring_bind)


let initialize =
  let definitions = [
    define;
    destructuring_bind;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Mk.callable c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
