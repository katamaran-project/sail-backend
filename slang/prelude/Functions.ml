open Base
open Exception
open EvaluationContext
open Monads.Notations.Star(EvaluationContext)
open Multimethods.Result.Notations


module M = Multimethods


let lambda args =
  match args with
  | []             -> raise @@ SlangError "ill-formed lambda"
  | [_]            -> raise @@ SlangError "ill-formed lambda"
  | params :: body -> begin
      let* env    = current_environment    in
      let  params = !!(M.list M.symbol params)
      and  body   = !!(M.map M.value body)
      in
      return @@ Value.Closure (env, params, body)
    end


let define args =
  let define_function form body =
    match form with
    | []                          -> raise @@ Multimethods.MultimethodError M.ArgumentTypeError
    | function_name :: parameters -> begin
        let* env = current_environment
        in
        let closure = Value.Closure (env, parameters, body)
        in
        let* () = add_binding function_name closure
        in
        return Value.Nil
      end

  and define_variable identifier body =
    match body with
    | [ expression ] -> begin
        let* value = Evaluation.evaluate expression
        in
        let* () = add_binding identifier value
        in
        return Value.Nil
      end
    | _     -> raise @@ SlangError "ill-formed define"

  in
  match args with
  | []             -> raise @@ SlangError "ill-formed define"
  | [_]            -> raise @@ SlangError "ill-formed define"
  | form :: body -> begin
      let=! body = M.map M.value body
      in
      match M.list M.symbol form with
      | M.Result.Success form_symbols -> define_function form_symbols body
      | M.Result.Failure M.ExecutionError -> raise @@ M.MultimethodError M.ExecutionError
      | M.Result.Failure M.ArgumentTypeError -> begin
          match M.symbol form with
          | M.Result.Success identifier -> define_variable identifier body
          | M.Result.Failure e          -> raise @@ M.MultimethodError e
        end
    end


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "lambda" lambda;
      native_function "define" define;
    )
