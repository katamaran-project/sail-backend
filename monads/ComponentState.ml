module type S = sig
  include Sig.Monad

  type state
    
  val get : (state -> 'a) -> 'a t
  val put : (state -> 'a -> state) -> 'a -> unit t
  val run : 'a t -> state -> ('a * state)
end

module Make (S : sig type t end) : (S with type state = S.t) = struct
  module M = State.Make(S)
      
  type state = M.state
  type 'a t  = 'a M.t

  let run    = M.run
  let return = M.return
  let bind   = M.bind

  let get f =
    let open Notations.Star(M)
    in
    let* s = M.get
    in
    return @@ f s

  let put f x =
    let open Notations.Star(M)
    in
    let* s = M.get
    in
    M.put @@ f s x
end
