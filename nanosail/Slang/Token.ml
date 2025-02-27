type t =
  | LeftParenthesis                  (* ( *)
  | RightParenthesis                 (* ) *)
  | Quote                            (* ' *)
  | Symbol           of string       (* foo *)
  | String           of string       (* "foo" *)
  | Integer          of int          (* 123 *)
  | True                             (* #t *)
  | False                            (* #f *)
