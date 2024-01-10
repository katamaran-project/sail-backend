module type S = sig
  include Sig.Monad

  type state
    
  val get : state t
  val put : state -> unit t
  val run : 'a t -> state -> ('a * state)
end

module Make (S : sig type t end) : (S with type state = S.t) = struct
  type state = S.t
                 
  type 'a t =
    | State of (state -> 'a * state)

  let run (State f) s =
    f s

  let return x =
    State (fun s -> (x, s))

  let bind (State f) g =
    State (fun s ->
        let (r, s') = f s
        in
        run (g r) s'
      )

  let get =
    State (fun s -> (s, s))

  let put s =
    State (fun _ -> ((), s))
end
