(*
   State monad where the state consists of multiple components
   which can be retrieved and updated separately.
*)

module type S = sig
  include Sig.Monad

  type state

  type 'a accessor = (state -> 'a) * (state -> 'a -> state)
  
  val get : 'a accessor -> 'a t
  val put : 'a accessor -> 'a -> unit t
  val act : (unit -> 'a) -> 'a t
  val run : 'a t -> state -> ('a * state)
end


module Make (S : sig type t end) : (S with type state = S.t) = struct
  module M = State.Make(S)

  type state       = M.state
  type 'a t        = 'a M.t
  type 'a accessor = (state -> 'a) * (state -> 'a -> state)

  let run    = M.run
  let return = M.return
  let bind   = M.bind

  let get ((f, _) : 'a accessor) : 'a t =
    let open Notations.Star(M)
    in
    let* s = M.get
    in
    return @@ f s

  let put
      ((_, f) : 'a accessor)
      (x      : 'a         ) : unit t
    =
    let open Notations.Star(M)
    in
    let* s = M.get
    in
    M.put @@ f s x

  let act = M.act
end
