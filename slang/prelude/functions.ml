open Base
open Util
open Exception
open Evaluation_context
open Monads.Notations.Star(Evaluation_context)


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
  match args with
  | []             -> raise @@ SlangError "ill-formed define"
  | [_]            -> raise @@ SlangError "ill-formed define"
  | params :: body -> begin
      let*  env    = current_environment in
      let=! params = T.list T.symbol params
      and=! body   = T.map T.value body
      in
      match params with
      | []                          -> raise @@ SlangError "ill-formed define"
      | function_name :: parameters -> begin
          let closure = V.Closure (env, parameters, body)
          in
          let* () = add_binding function_name closure
          in
          return V.Nil
        end
    end


let library env =
  extend_environment env (fun { native_function; _ } ->
      native_function "lambda" lambda;
      native_function "define" define;
    )
