open Base


exception EvaluationError of string


module Monad = Monads.State.Make(struct type t = Value.t Environment.t end)

module Notations = Monads.Notations.Star(Monad)

include Notations

type 'a t = 'a Monad.t

let current_environment =
  Monad.get

let set_current_environment =
  Monad.put

let bind identifier value =
  let* env = current_environment
  in
  let env' = Environment.bind env identifier value
  in
  Monad.put env'

let lookup identifier =
  let* env = current_environment
  in
  match Environment.lookup env identifier with
  | Some value -> Monad.return value
  | None       -> raise @@ EvaluationError ("unbound identifier " ^ identifier)

let return = Monad.return

let run = Monad.run

include Monads.Util.Make(Monad)
