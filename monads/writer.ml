module type S = sig
  include Sig.Monad
            
  type monoid

  val write : monoid -> unit t
  val run   : 'a t -> ('a * monoid)
end


module Make (M : Monoid.S) : (S with type monoid = M.t) = struct
  type monoid = M.t

  module MState = State.Make(M)

  type 'a t = 'a MState.t

  let return  = MState.return
  let bind    = MState.bind
  let run f   = MState.run f M.empty

  let write m =
    let open Notations.Star(MState)
    in
    let* state = MState.get
    in
    let state' = M.(state <+> m)
    in
    MState.put state'
end
