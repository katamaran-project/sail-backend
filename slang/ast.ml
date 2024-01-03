type ast =
  | Cons    of ast * ast
  | Integer of int
  | Symbol  of string
  | String  of string
  | Bool    of bool
  | Nil
