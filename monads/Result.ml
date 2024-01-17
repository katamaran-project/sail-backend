module type S = sig
  type error

  type 'a t =
    | Success of 'a
    | Failure of error

  val return : 'a -> 'a t
  val fail   : error -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  val (<|>)  : ('a -> 'b t) -> ('a -> 'b t) -> ('a -> 'b t)
end

module Make (E : sig type t end) : (S with type error = E.t) = struct
  type error = E.t
  
  type 'a t =
    | Success of 'a
    | Failure of error

  let return x = Success x

  let fail e = Failure e

  let bind x f =
    match x with
    | Success a -> f a
    | Failure e -> Failure e

  let (<|>) f g x =
    match f x with
    | Success x -> Success x
    | Failure _ -> begin
        match g x with
        | Success x -> Success x
        | Failure e -> Failure e
      end
end
