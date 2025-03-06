open Base


module type Source = sig
  type t
  type item

  val next : t -> (item * t) option
end


module MakeListSource (Item : sig type t end) : (Source with type item = Item.t and type t = Item.t list) = struct
  type item = Item.t
  type t    = item list

  let next xs =
    match xs with
    | []    -> None
    | x::xs -> Some (x, xs)
end


module MakeSequenceSource (Item : sig type t end) : (Source with type item = Item.t and type t = Item.t Sequence.t) = struct
  type item = Item.t
  type t    = item Sequence.t

  let next  = Sequence.next
end


module type S = sig
  include Sig.Monad

  module S : Source

  type item
  type source

  val current     : item option t
  val next        : unit t

  val run         : 'a t -> source -> ('a * source)
end


module Make (S : Source) : (S with type item = S.item and type source = S.t) = struct
  type item        = S.item
  type source      = S.t

  module S         = S
  module MState    = State.Make(struct type t = source end)

  type 'a t        = 'a MState.t

  let return       = MState.return
  let bind         = MState.bind
  let run f source = MState.run f source

  let current =
    let open Notations.Star(MState)
    in
    let* state = MState.get
    in
    return @@ Option.map ~f:fst (S.next state)

  let next =
    let open Notations.Star(MState)
    in
    let* state = MState.get
    in
    match S.next state with
    | Some (_, tail) -> MState.put tail
    | None           -> failwith "dispenser end reached"
end
