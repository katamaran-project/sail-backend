module type S = sig
  include Sig.Monad
            
  type item

  val write : item -> unit t
  val run   : 'a t -> ('a * item list)
end


module Make (M : sig type t end) : (S with type item = M.t) = struct
  type item = M.t

  module ListMonoid : (Monoid.S with type t = M.t list) = struct
    type t = M.t list

    let empty = []

    let ( <+> ) = List.append
  end

  module MWriter = Writer.Make(ListMonoid)

  type 'a t = 'a MWriter.t

  let return  = MWriter.return
  let bind    = MWriter.bind
  let run     = MWriter.run

  let write item =
    MWriter.write [item]
end
