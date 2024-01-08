open Base
open Exception
open Evaluation
open EvaluationContext
open Types.Notations
open Monads.Notations.Star(EvaluationContext)


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


let car args =
  match args with
  | [ argument ] ->
    let* argument' = evaluate argument
    in
    let=! (car, _) = T.cons T.value T.value argument'
    in
    return car
  | _ -> raise @@ SlangError "car expects one argument"


let cdr args =
  match args with
  | [ argument ] ->
    let* argument' = evaluate argument
    in
    let=! (_, cdr) = T.cons T.value T.value argument'
    in
    return cdr
  | _ -> raise @@ SlangError "cdr expects one argument"


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "cons" cons;
      native_function "car" car;
      native_function "cdr" cdr;
    )
