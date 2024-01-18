module type S = sig
  include Sig.Monad

  type state
  type error
  type 'a result

  val get : state t
  val put : state -> unit t
  val run : 'a t -> state -> ('a result * state)
end

module Make (S : sig type t end) (E : sig type t end) : (S with type state = S.t and type error = E.t) = struct
  module SM = ComponentState.Make(S)
  module RM = Result.Make(E)
  
  type    state  =    S.t
  type    error  =    E.t
  type 'a result = 'a RM.t

  type 'a t =
    | State of (state -> ('a RM.t) * state)

  let run (State f) s =
    f s

  let return x =
    State (fun s -> (RM.Success x, s))

  let bind (State f) g =
    State (fun s ->
        match f s with
        | (RM.Success a, s') -> run (g a) s'
        | (RM.Failure e, s') -> (RM.Failure e, s')
      )

  let get =
    State (fun s -> (RM.return s, s))

  let put s =
    State (fun _ -> (RM.return (), s))
end
