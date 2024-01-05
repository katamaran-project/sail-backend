type t =
  | Cons           of t * t
  | Integer        of int
  | Symbol         of string
  | String         of string
  | Bool           of bool
  | Nil
  | Closure        of t Environment.t * string list * t
  | NativeFunction of native_function

and native_function = t list -> t

let rec cons_to_list value =
  match value with
  | Nil               -> []
  | Cons (head, tail) -> head :: cons_to_list tail
  | _                 -> failwith "invalid list"
