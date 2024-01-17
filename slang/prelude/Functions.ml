open Base
open Exception
open EvaluationContext
open Monads.Notations.Star(EvaluationContext)
open Types.Result.Notations


module T = Types


let lambda args =
  match args with
  | []             -> raise @@ SlangError "ill-formed lambda"
  | [_]            -> raise @@ SlangError "ill-formed lambda"
  | params :: body -> begin
      let* env    = current_environment    in
      let  params = !!(T.list T.symbol params)
      and  body   = !!(T.map T.value body)
      in
      return @@ Value.Closure (env, params, body)
    end


let define args =
  let define_function form body =
    match form with
    | []                          -> raise @@ Types.MultimethodError Types.ArgumentTypeError
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
      let=! body = T.map T.value body
      in
      match T.list T.symbol form with
      | T.Result.Success form_symbols -> define_function form_symbols body
      | T.Result.Failure T.ExecutionError -> raise @@ T.MultimethodError T.ExecutionError
      | T.Result.Failure T.ArgumentTypeError -> begin
          match T.symbol form with
          | T.Result.Success identifier -> define_variable identifier body
          | T.Result.Failure e          -> raise @@ T.MultimethodError e
        end
    end


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "lambda" lambda;
      native_function "define" define;
    )
