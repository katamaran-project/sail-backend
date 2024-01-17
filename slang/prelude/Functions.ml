open Base
open Exception
open EvaluationContext
open Monads.Notations.Star(EvaluationContext)


module T = Types
module V = Value

open T.Notations


let lambda args =
  match args with
  | []             -> raise @@ SlangError "ill-formed lambda"
  | [_]            -> raise @@ SlangError "ill-formed lambda"
  | params :: body -> begin
      let*  env    = current_environment in
      let=! params = T.list T.symbol params
      and=! body   = T.map T.value body
      in
      return @@ V.Closure (env, params, body)
    end


let define args =
  let define_function form body =
    match form with
    | []                          -> raise @@ SlangError "ill-formed define"
    | function_name :: parameters -> begin
        let* env = current_environment
        in
        let closure = V.Closure (env, parameters, body)
        in
        let* () = add_binding function_name closure
        in
        return V.Nil
      end

  and define_variable identifier body =
    match body with
    | [ expression ] -> begin
        let* value = Evaluation.evaluate expression
        in
        let* () = add_binding identifier value
        in
        return V.Nil
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
      | Some form_symbols -> define_function form_symbols body
      | None              -> begin
          match T.symbol form with
          | Some identifier -> define_variable identifier body
          | None            -> raise @@ SlangError "ill-formed define"
        end
    end


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "lambda" lambda;
      native_function "define" define;
    )
