module type S = sig
  type error

  type 'a t =
    | Success of 'a
    | Failure of error

  val return : 'a -> 'a t
  val fail   : error -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  val disjunction : ('a -> 'b t) -> ('a -> 'b t) -> ('a -> 'b t)
  val force       : 'a t -> 'a
  val map    : 'a t -> f:('a -> 'b) -> 'b t
  val all    : 'a t list -> ('a list) t

  module Notations : sig
    val (<|>)   : ('a -> 'b t) -> ('a -> 'b t) -> ('a -> 'b t)
    val (!!)    : 'a t -> 'a
    val (|?>)   : ('a -> 'b t) -> ('b -> 'c) -> 'a -> 'c t

    val (let=!) : 'a t -> ('a -> 'b) -> 'b
    val (and=!) : 'a t -> 'b t -> 'a * 'b
  end
end

module Make (E : sig type t end) : (S with type error = E.t) = struct
  type error = E.t

  exception ResultError of error

  type 'a t =
    | Success of 'a
    | Failure of error

  let return x = Success x

  let fail e = Failure e

  let bind x f =
    match x with
    | Success a -> f a
    | Failure e -> Failure e

  let map x ~f =
    match x with
    | Success x -> Success (f x)
    | Failure e -> Failure e

  let all xs =
    let rec aux acc xs =
      match xs with
      | []                -> return @@ List.rev acc
      | (Success a) :: xs -> aux (a :: acc) xs
      | (Failure e) :: _  -> Failure e
    in
    aux [] xs

  let disjunction f g result =
    match f result with
    | Success x -> Success x
    | Failure _ -> begin
        match g result with
        | Success x -> Success x
        | Failure e -> Failure e
      end

  let force  result =
    match result with
    | Success x -> x
    | Failure e -> raise @@ ResultError e


  module Notations = struct
    let (<|>) = disjunction

    let (!!) = force

    let (|?>) f g =
      fun value -> map (f value) ~f:g

    let (let=!) x f =
      f (!! x)

    let (and=!) x y =
      let=! x in
      let=! y in
      (x, y)
  end
end
