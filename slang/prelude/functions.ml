open Base
open Util
open Evaluation_context
open Monads.Notations.Star(Evaluation_context)


module T = Types
module V = Value

open T.Notations
    

let lambda args =
  match args with
  | []             -> raise T.TypeError
  | [_]            -> raise T.TypeError
  | params :: body -> begin
      let*  env = current_environment
      in
      let=! params = T.list T.symbol params
      and=! body = T.map T.value body
      in    
      return @@ V.Closure (env, params, body)
    end


let library env =
  extend_environment env (fun { native_function; _ } ->
      native_function "lambda" lambda
    )
