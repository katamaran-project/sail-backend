type t =
  | LeftParenthesis
  | RightParenthesis
  | Quote
  | Symbol           of string
  | String           of string
  | Integer          of int
  | True
  | False
