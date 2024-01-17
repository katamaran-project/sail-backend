open Base
open Exception
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module M = Multimethods


let lambda args =
  match args with
  | []             -> raise @@ SlangError "ill-formed lambda"
  | [_]            -> raise @@ SlangError "ill-formed lambda"
  | params :: body -> begin
      let* env    = EV.current_environment    in
      let  params = M.list M.symbol params
      and  body   = List.map ~f:M.value body
      in
      EV.return @@ Value.Closure (env, params, body)
    end


let define args =
  let define_function form body =
    match form with
    | []                          -> raise M.DispatchFailure
    | function_name :: parameters -> begin
        let* env = EV.current_environment
        in
        let closure = Value.Closure (env, parameters, body)
        in
        let* () = EV.add_binding function_name closure
        in
        EV.return Value.Nil
      end

  and define_variable identifier body =
    match body with
    | [ expression ] -> begin
        let* value = Evaluation.evaluate expression
        in
        let* () = EV.add_binding identifier value
        in
        EV.return Value.Nil
      end
    | _     -> raise @@ SlangError "ill-formed define"

  in
  match args with
  | []             -> raise @@ SlangError "ill-formed define"
  | [_]            -> raise @@ SlangError "ill-formed define"
  | form :: body -> begin
      let body = List.map ~f:M.value body
      in
      try
        let form_symbols = M.list M.symbol form
        in
        define_function form_symbols body
      with
      | M.DispatchFailure -> begin
          let identifier = M.symbol form
          in
          define_variable identifier body
        end
    end


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "lambda" lambda;
      native_function "define" define;
    )
