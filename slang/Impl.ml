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

  val cons_to_list : t -> t list
  val to_string    : t -> string
end = struct
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


  let rec to_string value =
    match value with
    | Cons (_, _)        -> "(" ^ (String.concat " " @@ List.map to_string (cons_to_list value)) ^ ")"
    | Integer n          -> Int.to_string n
    | Symbol s           -> s
    | String s           -> Printf.sprintf "\"%s\"" s
    | Bool true          -> "#t"
    | Bool false         -> "#f"
    | Nil                -> "()"
    | Closure (_, _, _)  -> "<closure>"
    | NativeFunction _   -> "<native function>"
end

and EvaluationContext : sig
  type state = Value.t Environment.t

  type 'a t

  val return                  : 'a -> 'a t
  val current_environment     : Value.t Environment.t t
  val set_current_environment : Value.t Environment.t -> unit t
  val bind                    : string -> Value.t -> unit t
  val lookup                  : string -> Value.t option t
  val run                     : 'a t -> state -> 'a * state

  val map                     : ('a -> 'b t) -> 'a list -> 'b list t
  val iter                    : ('a -> unit t) -> 'a list -> unit t

  module Notations : sig
    val (let*)                  : 'a t -> ('a -> 'b t) -> 'b t
    val (and*)                  : 'a t -> 'b t -> ('a * 'b) t
  end
end = struct
  type state = Value.t Environment.t

  module Monad = Monads.State.Make(struct type t = state end)

  module Notations = Monads.Notations.Star(Monad)

  include Monads.Util.Make(Monad)


  type 'a t = 'a Monad.t

  let return = Monad.return

  let current_environment =
    Monad.get

  let set_current_environment =
    Monad.put

  let bind identifier value =
    let open Notations
    in
    let* env = current_environment
    in
    let env' = Environment.bind env identifier value
    in
    Monad.put env'

  let lookup identifier =
    let open Notations
    in
    let* env = current_environment
    in
    return @@ Environment.lookup env identifier

  let run = Monad.run
end
