open Base
open Exception
open Monads.Notations.Star(EvaluationContext)
open Multimethods

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
        let*   env    = EC.current_environment   in
        let=!  params = C.list C.symbol params   in
        let=!! body   = List.map ~f:C.value body
        in
        EV.return @@ Value.Callable (Evaluation.mk_closure env params body)
      end
  in
  (id, impl)


let define =
  let id = "define"
    
  and define_function (args : Value.t list) =
    match args with
    | form :: body -> begin
        let=? function_name, parameters = C.(cons symbol (list symbol)) form
        in
        let* env      = EV.current_environment
        in
        let  callable = Value.Callable (Evaluation.mk_closure env parameters body)
        in
        let* ()       = EV.add_binding function_name callable
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
  (id, mk_multi_special_form id [ define_function; define_variable ])
    


let library env =
  let definitions = [
    lambda;
    define;
  ]
  in
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      List.iter
        ~f:(Auxlib.uncurry callable)
        definitions
    )


let initialize =
  let definitions = [
    lambda;
    define;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Auxlib.uncurry EC.add_binding) pairs
