open Base


module rec Value : sig
  type t =
    | Cons           of t * t
    | Integer        of int
    | Symbol         of string
    | String         of string
    | Bool           of bool
    | Nil
    | Closure        of t Environment.t * string list * t list
    | NativeFunction of native_function

  and native_function = t list -> t EvaluationContext.t

  val equal        : t -> t -> bool

  val cons_to_list : t -> t list
  val list_to_cons : t list -> t
  val to_string    : t -> string
end
=
struct
  type t =
    | Cons           of t * t
    | Integer        of int
    | Symbol         of string
    | String         of string
    | Bool           of bool
    | Nil
    | Closure        of t Environment.t * string list * t list
    | NativeFunction of native_function

  and native_function = t list -> t EvaluationContext.t

  let rec cons_to_list value =
    match value with
    | Nil               -> []
    | Cons (head, tail) -> head :: cons_to_list tail
    | _                 -> failwith "invalid list"

  let rec list_to_cons values =
    match values with
    | []       -> Nil
    | x::xs    -> Cons (x, list_to_cons xs)

  let rec to_string value =
    match value with
    | Cons (_, _)          -> "(" ^ (String.concat ~sep:" " @@ List.map ~f:to_string (cons_to_list value)) ^ ")"
    | Integer n            -> Int.to_string n
    | Symbol s             -> s
    | String s             -> Printf.sprintf "\"%s\"" s
    | Bool true            -> "#t"
    | Bool false           -> "#f"
    | Nil                  -> "()"
    | Closure (_, _, body) -> Printf.sprintf "(<closure> %s)" (String.concat ~sep:" " @@ List.map ~f:to_string body)
    | NativeFunction _     -> "<native function>"

  (* Deal with all cases explicitly so as to get a compiler error when we add more value types *)
  let rec equal (v1 : t) (v2 : t) : bool =
    match v1 with
    | Cons (h1, t1) -> begin
        match v2 with
        | Cons (h2, t2)           -> equal h1 h2 && equal t1 t2
        | Integer _               -> false
        | Symbol _                -> false
        | String _                -> false
        | Bool _                  -> false
        | Nil                     -> false
        | Closure (_, _, _)       -> false
        | NativeFunction _        -> false
      end
    | Integer n1 -> begin
        match v2 with
        | Cons (_, _)             -> false
        | Integer n2              -> Int.equal n1 n2
        | Symbol _                -> false
        | String _                -> false
        | Bool _                  -> false
        | Nil                     -> false
        | Closure (_, _, _)       -> false
        | NativeFunction _        -> false
      end
    | Symbol s1 -> begin
      match v2 with
       | Cons (_, _)              -> false
       | Integer _                -> false
       | Symbol s2                -> String.equal s1 s2
       | String _                 -> false
       | Bool _                   -> false
       | Nil                      -> false
       | Closure (_, _, _)        -> false
       | NativeFunction _         -> false
    end
    | String s1 -> begin
        match v2 with
        | Cons (_, _)             -> false
        | Integer _               -> false
        | Symbol _                -> false
        | String s2               -> String.equal s1 s2
        | Bool _                  -> false
        | Nil                     -> false
        | Closure (_, _, _)       -> false
        | NativeFunction _        -> false
      end
    | Bool b1 -> begin
      match v2 with
       | Cons (_, _)              -> false
       | Integer _                -> false
       | Symbol _                 -> false
       | String _                 -> false
       | Bool b2                  -> Bool.equal b1 b2
       | Nil                      -> false
       | Closure (_, _, _)        -> false
       | NativeFunction _         -> false
    end
    | Nil -> begin
        match v2 with
        | Cons (_, _)             -> false
        | Integer _               -> false
        | Symbol _                -> false
        | String _                -> false
        | Bool _                  -> false
        | Nil                     -> true
        | Closure (_, _, _)       -> false
        | NativeFunction _        -> false
      end
    | Closure (_, _, _)           -> false
    | NativeFunction _            -> false
end

and EvaluationContext : sig
  type state = Value.t Environment.t

  type 'a t

  val return                  : 'a -> 'a t
  val bind                    : 'a t -> ('a -> 'b t) -> 'b t
  val current_environment     : Value.t Environment.t t
  val set_current_environment : Value.t Environment.t -> unit t
  val current_state           : state t
  val set_current_state       : state -> unit t
  val add_binding             : string -> Value.t -> unit t
  val lookup                  : string -> Value.t option t
  val run                     : 'a t -> state -> 'a * state

  val map                     : ('a -> 'b t) -> 'a list -> 'b list t
  val iter                    : ('a -> unit t) -> 'a list -> unit t
end
=
struct
  type state = Value.t Environment.t

  module Monad = Monads.State.Make(struct type t = state end)

  include Monads.Util.Make(Monad)


  type 'a t = 'a Monad.t

  let return                  = Monad.return
  let bind                    = Monad.bind
  let current_environment     = Monad.get
  let set_current_environment = Monad.put
  let current_state           = Monad.get
  let set_current_state       = Monad.put

  let add_binding identifier value =
    let open Monads.Notations.Star(Monad)
    in
    let* env = current_environment
    in
    let env' = Environment.bind env identifier value
    in
    Monad.put env'

  let lookup identifier =
    let open Monads.Notations.Star(Monad)
    in
    let* env = current_environment
    in
    return @@ Environment.lookup env identifier

  let run = Monad.run
end
