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
      EV.return @@ Value.Closure (env, params, body)
    end


let define (args : Value.t list) : Value.t EV.t =
  let define_function (args : Value.t list) =
    match args with
    | form :: body -> begin
        let=?  function_name, parameters = M.cons M.symbol (M.list M.symbol) form
        in
        let* env     = EV.current_environment                in
        let  closure = Value.Closure (env, parameters, body) in
        let* ()      = EV.add_binding function_name closure
        in
        EV.return @@ Some (Value.Nil)
      end
    | _ -> EV.return None

  and define_variable (args : Value.t list) =
    let=? identifier, expression = M.map2 M.symbol M.value args
    in
    let* value = Evaluation.evaluate expression
    in
    let* () = EV.add_binding identifier value
    in
    EV.return @@ Some (Value.Nil)

  in
  M.mk_multimacro [ define_function; define_variable ] args


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "lambda" lambda;
      native_function "define" define;
    )
