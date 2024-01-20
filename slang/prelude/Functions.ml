open Base
open Exception
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module M = Multimethods

open Shared


let lambda (args : Value.t list) : Value.t EV.t =
  match args with
  | []             -> raise @@ SlangError "ill-formed lambda"
  | [_]            -> raise @@ SlangError "ill-formed lambda"
  | params :: body -> begin
      let*   env    = EV.current_environment   in
      let=!  params = M.list M.symbol params   in
      let=!! body   = List.map ~f:M.value body
      in
      EV.return @@ Value.Callable (Evaluation.mk_closure env params body)
    end


let define (args : Value.t list) : Value.t EV.t =
  let define_function (args : Value.t list) =
    match args with
    | form :: body -> begin
        let=? function_name, parameters = M.(cons symbol (list symbol)) form
        in
        let* env      = EV.current_environment
        in
        let  callable = Value.Callable (Evaluation.mk_closure env parameters body)
        in
        let* ()       = EV.add_binding function_name callable
        in
        EV.return @@ Some (Value.Nil)
      end
    | _ -> EV.return None

  and define_variable (args : Value.t list) =
    let=? identifier, expression = M.(map2 symbol value) args
    in
    let* value = Evaluation.evaluate expression
    in
    let* () = EV.add_binding identifier value
    in
    EV.return @@ Some (Value.Nil)

  in
  M.mk_multimacro [ define_function; define_variable ] args


let library env =
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      callable "lambda" lambda;
      callable "define" define;
    )
