open! ExtBase
open Monads.Notations.Star(EvaluationContext)

module EC = EvaluationContext


let (let=?) x f =
  match x with
  | Some r -> f r
  | None   -> EC.return None


let (let=??) xs f =
  match Option.all xs with
  | Some r -> f r
  | None   -> EC.return None


let (let=!) x f =
  match x with
  | Some r -> f r
  | None   -> raise @@ Exception.SlangError "invalid types"


let (let=!!) xs f =
  match Option.all xs with
  | Some r -> f r
  | None   -> raise @@ Exception.SlangError "invalid types"


let bind_callable id callable =
  EC.add_binding id @@ Value.Callable callable


let error identifier args =
  let error_message =
    let formatted_args =
      String.concat ~sep:" " @@ List.map ~f:Value.to_string args
    in
    Printf.sprintf "error evaluating (%s %s)" identifier formatted_args
  in
  raise @@ Exception.SlangError error_message
