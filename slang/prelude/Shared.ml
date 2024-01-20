open Base
open Monads.Notations.Star(EvaluationContext)

module EV = EvaluationContext
module M  = Multimethods


let (let=?) x f =
  match x with
  | Some r -> f r
  | None   -> EV.return None


let (let=??) xs f =
  match Option.all xs with
  | Some r -> f r
  | None   -> EV.return None


let (let=!) x f =
  match x with
  | Some r -> f r
  | None   -> raise @@ Exception.SlangError "invalid types"


let (let=!!) xs f =
  match Option.all xs with
  | Some r -> f r
  | None   -> raise @@ Exception.SlangError "invalid types"
