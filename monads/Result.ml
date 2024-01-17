module type S = sig
  include Sig.Monad

  type error    
end

module Make (E : sig type t end) : (S with type error = E.t) = struct
  type error = E.t
  
  type 'a t =
    | Success of 'a
    | Failure of error

  let return x = Success x

  let bind x f =
    match x with
    | Success a -> f a
    | Failure e -> Failure e
end
