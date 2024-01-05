type value =
  | Cons    of value * value
  | Integer of int
  | Symbol  of string
  | String  of string
  | Bool    of bool
  | Nil
