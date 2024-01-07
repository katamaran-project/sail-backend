module type Source = sig
  type t
  type item

  val next : t -> (item * t) option
end

module type S = sig
  include Sig.Monad

  type item

  val current     : item option t
  val next        : unit t
      
  val run         : 'a t -> ('a * item)
end


(* module Make (S : Source) : (S with type item = S.item) = struct *)
module Make (S : Source) = struct
  type item = S.item

  module MState = State.Make(struct type t = (item * S.t) option end)

  type 'a t = 'a MState.t

  let return  = MState.return
  let bind    = MState.bind
  let run     = MState.run

  let current =
    let open Monads.Notations.Star(MState)
    in
    let* state = MState.get
    in
    return @@ Option.map fst state
  
  let next =
    let open Monads.Notations.Star(MState)
    in
    let* state = MState.get
    in
    match state with
    | None             -> failwith "reader reached end"
    | Some (_, source) -> MState.put @@ S.next source
end
