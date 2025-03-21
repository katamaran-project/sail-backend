(*

   Monad that combines the component state monad and result monad.

*)

module type S = sig
  include Sig.Monad

  type state
  type error
  type 'a accessor = (state -> 'a) * (state -> 'a -> state)
  type 'a result =
    | Success of 'a
    | Failure of error

  val get     : 'a accessor -> 'a t
  val put     : 'a accessor -> 'a -> unit t
  val act     : (unit -> 'a) -> 'a t
  val update  : 'a accessor -> ('a -> 'a) -> unit t
  val run     : 'a t -> state -> ('a result * state)
  val fail    : error -> 'a t
  val recover : 'a t -> (error -> 'a t) -> 'a t
end


module Make (S : sig type t end) (E : sig type t end) : (S with type state = S.t and type error = E.t) = struct
  module SM = ComponentState.Make(S)
  module RM = Result.Make(E)

  type 'a t        = 'a RM.t SM.t
  type    state    =    S.t
  type    error    =    E.t
  type 'a result   = 'a RM.t         = Success of 'a | Failure of error   (* needed to prevent result from becoming abstract *)
  type 'a accessor = 'a SM.accessor

  let run = SM.run

  let return (x : 'a) : 'a t =
    Base.Fn.compose SM.return RM.return x

  let fail (error : error) : 'a t =
    SM.return @@ RM.Failure error

  let bind (f : 'a t) (g : 'a -> 'b t) : 'b t =
    SM.bind f (fun x ->
        match x with
        | RM.Success x -> g x
        | RM.Failure e -> SM.return @@ RM.Failure e
      )

  let get (accessor : 'a accessor) : 'a t =
    SM.bind (SM.get accessor) return

  let put
      (accessor : 'a accessor)
      (x        : 'a         ) : unit t
    =
    SM.bind (SM.put accessor x) return

  let act (action : unit -> 'a) : 'a t =
    SM.bind (SM.act action) return

  let update
      (accessor : 'a accessor)
      (f        : 'a -> 'a   ) : unit t
    =
    bind (get accessor) @@ fun x -> put accessor (f x)

  let recover
      (f             : 'a t         )
      (error_handler : error -> 'a t) : 'a t
    =
    SM.bind f (fun result ->
        match result with
        | Success _ -> SM.return result
        | Failure e -> error_handler e
      )
end
