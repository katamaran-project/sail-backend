open Base
open Exception
open Evaluation
open Monads.Notations.Star(EvaluationContext)


module EV = EvaluationContext
module M = Multimethods
module V = Value


let cons args =
  match args with
  | car :: cdr :: [] ->
    let* car' = evaluate car
    and* cdr' = evaluate cdr
    in
    EV.return @@ V.Cons (car', cdr')
  | _ -> raise @@ SlangError "ill-formed cons"


let car args =
  match args with
  | [ argument ] -> begin
      let* argument' = evaluate argument
      in
      let (car, _) = M.cons M.value M.value argument'
      in
      EV.return car
    end
  | _ -> raise @@ SlangError "car expects one argument"


let cdr args =
  match args with
  | [ argument ] -> begin
      let* argument' = evaluate argument
      in
      let (_, cdr) = M.cons M.value M.value argument'
      in
      EV.return cdr
    end
  | _ -> raise @@ SlangError "cdr expects one argument"


let library env =
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "cons" cons;
      native_function "car" car;
      native_function "cdr" cdr;
    )
