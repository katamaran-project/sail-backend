module type StateArg = sig
  type t
end

module type S = sig
  include Sig.Monad

  type state
    
  val get : state t

  val put : state -> unit t

  val run : 'a t -> state -> ('a * state)
end


module Make (S : StateArg) : (S with type state = S.t) = struct
  type state = S.t
                 
  type 'a t =
    | State of (state -> 'a * state)

  let run (State f) s =
    f s

  let return x =
    State (fun s -> (x, s))

  let bind (State f) g =
    State (fun s ->
        let (r, s') = f s
        in
        run (g r) s'
      )

  let get =
    State (fun s -> (s, s))

  let put s =
    State (fun _ -> ((), s))
end


module Util (M : S) = struct
  let rec collect n f =
    if n <= 0
    then M.return []
    else
      M.bind f (fun r -> M.bind (collect (n-1) f) (fun rs -> M.return @@ r::rs))

  let map f xs =
    let rec aux xs acc =
      match xs with
      | []    -> M.return @@ List.rev acc
      | x::xs -> M.bind (f x) (fun x' -> aux xs (x' :: acc))
    in
    aux xs []
end
