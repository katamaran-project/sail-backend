open Base


module rec Value : sig
  type t =
    | Cons           of t * t
    | Integer        of int
    | Symbol         of string
    | String         of string
    | Bool           of bool
    | Nil
    | Callable       of callable

  and callable = t list -> t EvaluationContext.t

  val equal        : t -> t -> bool

  val cons_to_list : t -> t list option
  val list_to_cons : t list -> t
  val to_string    : t -> string
  val truthy       : t -> bool

  module Mk : sig
    val cons            : t -> t   -> t
    val integer         : int      -> t
    val symbol          : string   -> t
    val string          : string   -> t
    val bool            : bool     -> t
    val callable        : callable -> t
  end

  module Predicate : sig
    val is_cons      : t -> bool
    val is_integer   : t -> bool
    val is_symbol    : t -> bool
    val is_string    : t -> bool
    val is_bool      : t -> bool
    val is_nil       : t -> bool
    val is_callable  : t -> bool
  end
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
    | Callable       of callable

  and callable = t list -> t EvaluationContext.t

  let cons_to_list value =
    let rec aux acc value =
      match value with
      | Nil               -> Some (List.rev acc)
      | Cons (head, tail) -> aux (head :: acc) tail
      | _                 -> None
    in
    aux [] value

  let rec list_to_cons values =
    match values with
    | []       -> Nil
    | x::xs    -> Cons (x, list_to_cons xs)

  let rec to_string value =
    match value with
    | Integer n            -> Int.to_string n
    | Symbol s             -> s
    | String s             -> Printf.sprintf "\"%s\"" s
    | Bool true            -> "#t"
    | Bool false           -> "#f"
    | Nil                  -> "()"
    | Callable _           -> "<callable>"
    | Cons (car, cdr)      -> begin
        match cons_to_list value with
        | Some vs -> "(" ^ (String.concat ~sep:" " @@ List.map ~f:to_string vs) ^ ")"
        | None    -> Printf.sprintf "(cons %s %s)" (to_string car) (to_string cdr)
      end


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
        | Callable _              -> false
      end
    | Integer n1 -> begin
        match v2 with
        | Cons (_, _)             -> false
        | Integer n2              -> Int.equal n1 n2
        | Symbol _                -> false
        | String _                -> false
        | Bool _                  -> false
        | Nil                     -> false
        | Callable _              -> false
      end
    | Symbol s1 -> begin
        match v2 with
        | Cons (_, _)              -> false
        | Integer _                -> false
        | Symbol s2                -> String.equal s1 s2
        | String _                 -> false
        | Bool _                   -> false
        | Nil                      -> false
        | Callable _               -> false
      end
    | String s1 -> begin
        match v2 with
        | Cons (_, _)             -> false
        | Integer _               -> false
        | Symbol _                -> false
        | String s2               -> String.equal s1 s2
        | Bool _                  -> false
        | Nil                     -> false
        | Callable _              -> false
      end
    | Bool b1 -> begin
        match v2 with
        | Cons (_, _)              -> false
        | Integer _                -> false
        | Symbol _                 -> false
        | String _                 -> false
        | Bool b2                  -> Bool.equal b1 b2
        | Nil                      -> false
        | Callable _               -> false
      end
    | Nil -> begin
        match v2 with
        | Cons (_, _)             -> false
        | Integer _               -> false
        | Symbol _                -> false
        | String _                -> false
        | Bool _                  -> false
        | Nil                     -> true
        | Callable _              -> false
      end
    | Callable _ -> false


  let falsey (value : t) =
    equal value @@ Bool false

  let truthy (value : t) =
    not @@ falsey value

  module Mk = struct
    let symbol   s   = Symbol s
    let string   s   = String s
    let cons     x y = Cons (x, y)
    let integer  n   = Integer n
    let bool     b   = Bool b
    let callable f   = Callable f
  end

  module Predicate = struct
    let is_cons value =
      match value with
      | Cons _           -> true
      | _                -> false

    let is_integer value =
      match value with
      | Integer _        -> true
      | _                -> false

    let is_symbol value =
      match value with
      | Symbol _         -> true
      | _                -> false

    let is_string value =
      match value with
      | String _         -> true
      | _                -> false

    let is_bool value =
      match value with
      | Bool _           -> true
      | _                -> false

    let is_nil value =
      match value with
      | Nil              -> true
      | _                -> false

    let is_callable value =
      match value with
      | Callable _       -> true
      | _                -> false
  end
end

and EvaluationContext : sig
  type state = Value.t Environment.t

  type 'a t

  val empty_state             : state

  val return                  : 'a -> 'a t
  val ignore                  : 'a t -> unit t
  val bind                    : 'a t -> ('a -> 'b t) -> 'b t
  val lift                    : f:('a -> 'b) -> 'a t -> 'b t
  val run_with_state          : 'a t -> state -> 'a * state
  val run                     : 'a t -> 'a * state

  val current_environment     : Value.t Environment.t t
  val set_current_environment : Value.t Environment.t -> unit t
  val current_state           : state t
  val set_current_state       : state -> unit t
  val add_binding             : string -> Value.t -> unit t
  val lookup                  : string -> Value.t option t

  val map                     : f:('a -> 'b t) -> 'a list -> 'b list t
  val iter                    : f:('a -> unit t) -> 'a list -> unit t
  val exists                  : f:('a -> bool t) -> 'a list -> bool t
  val forall                  : f:('a -> bool t) -> 'a list -> bool t
  val sequence                : 'a t list -> unit t
end
=
struct
  type state = Value.t Environment.t

  module Monad = Monads.State.Make(struct type t = state end)

  include Monads.Util.Make(Monad)


  type 'a t = 'a Monad.t

  let empty_state             = Environment.empty

  let return                  = Monad.return
  let bind                    = Monad.bind
  let ignore x                = bind x @@ fun _ -> return ()

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

  let run_with_state = Monad.run

  let run p = run_with_state p empty_state
end
