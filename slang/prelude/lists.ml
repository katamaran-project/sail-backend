open Base
open Exception
open Evaluation
open Evaluation_context
open Monads.Notations.Star(Evaluation_context)


module T = Types
module V = Value


let cons args =
  match args with
  | car :: cdr :: [] ->
    let* car' = evaluate car
    and* cdr' = evaluate cdr
    in
    return @@ V.Cons (car', cdr')
  | _ -> raise @@ SlangError "ill-formed cons"


let library env =
  Environment_builder.extend_environment env (fun { native_function; _ } ->
      native_function "cons" cons;
    )
