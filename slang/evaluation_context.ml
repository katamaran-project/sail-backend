include Impl.EvaluationContext


(* open Base *)


(* exception EvaluationError of string *)



(* module Make(Value : sig type t end) = struct *)

(*   type state = Value.t Environment.t *)

(*   module Monad = Monads.State.Make(struct type t = state end) *)

(*   include Monads.Notations.Star(Monad) *)

(*   include Monads.Util.Make(Monad) *)


(*   type 'a t = 'a Monad.t *)
  
(*   let current_environment = *)
(*     Monad.get *)

(*   let set_current_environment = *)
(*     Monad.put *)

(*   let bind identifier value = *)
(*     let* env = current_environment *)
(*     in *)
(*     let env' = Environment.bind env identifier value *)
(*     in *)
(*     Monad.put env' *)

(*   let lookup identifier = *)
(*     let* env = current_environment *)
(*     in *)
(*     match Environment.lookup env identifier with *)
(*     | Some value -> Monad.return value *)
(*     | None       -> raise @@ EvaluationError ("unbound identifier " ^ identifier) *)

(*   let return = Monad.return *)

(*   let run = Monad.run *)
(* end *)
