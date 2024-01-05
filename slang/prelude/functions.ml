open Util
open Value
open Types
    

let lambda env args =
  match args with
  | []             -> raise TypeError
  | [_]            -> raise TypeError
  | params :: body ->
    let=! params = list symbol params
    and=! body = map value body
    in    
    Closure (env, params, body)


let library env =
  extend_environment env (fun { native_function; _ } ->
      native_function "lambda" lambda
    )
