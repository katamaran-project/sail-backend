(*
    Groups mutually recursive modules.

    These modules should not be accessed directly, but rather through their aliases.
*)
module E = Error
open ExtBase
module Error = E


module rec Value : sig
  type t =
    | Cons           of t * t
    | Integer        of int
    | Symbol         of string
    | String         of string
    | Bool           of bool
    | Nil
    | Callable       of callable
    | Reference      of Address.t

  and callable = t list -> t EvaluationContext.t

  val equal        : t -> t -> bool

  val cons_to_list : t -> t list option
  val list_to_cons : t list -> t
  val to_string    : t -> string
  val truthy       : t -> bool
  val is_keyword   : string -> bool

  module Mk : sig
    val cons            : t -> t    -> t
    val integer         : int       -> t
    val symbol          : string    -> t
    val string          : string    -> t
    val bool            : bool      -> t
    val callable        : callable  -> t
    val reference       : Address.t -> t

    val nil             : t
  end

  module Predicate : sig
    val is_cons           : t -> bool
    val is_integer        : t -> bool
    val is_symbol         : t -> bool
    val is_keyword_symbol : t -> bool
    val is_string         : t -> bool
    val is_bool           : t -> bool
    val is_nil            : t -> bool
    val is_callable       : t -> bool
    val is_reference      : t -> bool
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
    | Reference      of Address.t

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
    | Reference _          -> "<ref>"
    | Cons (car, cdr)      -> begin
        match cons_to_list value with
        | Some vs -> "(" ^ (String.concat ~sep:" " @@ List.map ~f:to_string vs) ^ ")"
        | None    -> Printf.sprintf "(cons %s %s)" (to_string car) (to_string cdr)
      end


  let rec equal (v1 : t) (v2 : t) : bool =
    match v1 with
    | Cons (h1, t1) -> begin
        match v2 with
        | Cons (h2, t2)   -> equal h1 h2 && equal t1 t2
        | _               -> false
      end
    | Integer n1 -> begin
        match v2 with
        | Integer n2      -> Int.equal n1 n2
        | _               -> false
      end
    | Symbol s1 -> begin
        match v2 with
        | Symbol s2       -> String.equal s1 s2
        | _               -> false
      end
    | String s1 -> begin
        match v2 with
        | String s2       -> String.equal s1 s2
        | _               -> false
      end
    | Bool b1 -> begin
        match v2 with
        | Bool b2         -> Bool.equal b1 b2
        | _               -> false
      end
    | Nil -> begin
        match v2 with
        | Nil             -> true
        | _               -> false
      end
    | Reference x -> begin
        match v2 with
        | Reference y     -> Address.equal x y
        | _               -> false
      end
    | Callable _          -> false


  let falsey (value : t) =
    equal value (Bool false) || equal value Nil

  let truthy (value : t) =
    not @@ falsey value

  let is_keyword (id : string) =
    String.is_prefix ~prefix:":" id

  module Mk = struct
    let symbol    s   = Symbol s
    let string    s   = String s
    let cons      x y = Cons (x, y)
    let integer   n   = Integer n
    let bool      b   = Bool b
    let callable  f   = Callable f
    let reference a   = Reference a

    let nil          = Nil
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

    let is_keyword_symbol value =
      match value with
      | Symbol id -> is_keyword id
      | _         -> false

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

    let is_reference value =
      match value with
      | Reference _      -> true
      | _                -> false
  end
end

and State : sig
  type 'a accessor = (State.t -> 'a) * (State.t -> 'a -> State.t)

  type environment = Value.t Environment.t
  type heap        = Value.t Heap.t
  type t           = environment * heap

  val empty_state  : t

  val state                   : t accessor
  val environment             : environment accessor
  val heap                    : heap accessor
end = struct
  type 'a accessor = (State.t -> 'a) * (State.t -> 'a -> State.t)

  type environment = Value.t Environment.t
  type heap        = Value.t Heap.t
  type t           = environment * heap

  let empty_state : t =
    (Environment.empty, Heap.empty)

  let state                              = Monads.Accessors.id
  let environment : environment accessor = Monads.Accessors.(Pair.first id)
  let heap        : heap accessor        = Monads.Accessors.(Pair.second id)
end

and EvaluationContext : sig
  type 'a result =
    | Success of 'a
    | Failure of Error.t

  type 'a t

  val return                  : 'a -> 'a t
  val fail                    : Error.t -> 'a t
  val ignore                  : 'a t -> unit t
  val bind                    : 'a t -> ('a -> 'b t) -> 'b t
  val lift                    : f:('a -> 'b) -> 'a t -> 'b t
  val run_with_state          : 'a t -> State.t -> 'a result * State.t
  val run                     : 'a t -> 'a result * State.t

  (* State related functions *)
  val get                     : 'a State.accessor -> 'a t
  val put                     : 'a State.accessor -> 'a -> unit t

  (* Environment related functions *)
  val add_binding             : string -> Value.t -> unit t
  val lookup                  : string -> Value.t option t

  (* Heap related functions *)
  val heap_allocate           : Value.t -> Address.t t
  val heap_access             : Address.t -> Value.t t
  val heap_update             : Address.t -> Value.t -> unit t

  (* Useful extra functionality *)
  val map                     : f:('a -> 'b t) -> 'a list -> 'b list t
  val iter                    : f:('a -> unit t) -> 'a list -> unit t
  val exists                  : f:('a -> bool t) -> 'a list -> bool t
  val forall                  : f:('a -> bool t) -> 'a list -> bool t
  val sequence                : 'a t list -> 'a list t
end
=
struct
  module Result = Monads.Result.Make(Error)

  type 'a result
    = 'a Result.t
    = | Success of 'a         (* make it transparent *)
      | Failure of Error.t

  module Monad = Monads.StateResult.Make(State) (Error)

  open Monads.Notations.Star(Monad)

  include Monads.Util.Make(Monad)


  type 'a t = 'a Monad.t

  let return      = Monad.return
  let fail        = Monad.fail
  let bind        = Monad.bind
  let ignore x    = bind x @@ fun _ -> return ()

  let get         = Monad.get
  let put         = Monad.put

  let add_binding identifier value =
    let* env = get State.environment
    in
    let env' = Environment.bind env identifier value
    in
    put State.environment env'

  let lookup (identifier : string) : Value.t option t =
    let* env = get State.environment
    in
    return @@ Environment.lookup env identifier

  let heap_allocate (initial_value : Value.t) : Address.t t =
    let* original_heap = get State.heap
    in
    let (updated_heap, address) = Heap.allocate original_heap initial_value
    in
    let* () = put State.heap updated_heap
    in
    return address

  let heap_access (address : Address.t) : Value.t t =
    let* current_heap = get State.heap
    in
    match Heap.read current_heap address with
    | Some value -> return value
    | None       -> failwith "Bug, should not happen; somehow an address was forged"

  let heap_update (address : Address.t) (value : Value.t) : unit t =
    let* original_heap = get State.heap
    in
    let updated_heap = Heap.write original_heap address value
    in
    put State.heap updated_heap

  let run_with_state (f : 'a t) (state : State.t) : 'a result * State.t =
    let result, state' = Monad.run f state
    in
    match result with
    | Monad.Success value -> (Success value, state')
    | Monad.Failure error -> (Failure error, state')

  let run p = run_with_state p State.empty_state
end
