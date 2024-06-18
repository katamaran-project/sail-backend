type t =
  | Constant of Z.t
  | Add      of t * t
  | Minus    of t * t
  | Times    of t * t
  | Neg      of t
  | Id       of Identifier.t
  | Var      of Identifier.t

let rec to_string (numeric_expression : t) =
  match numeric_expression with
  | Constant n     -> Z.to_string n
  | Add   (e1, e2) -> Printf.sprintf "(%s + %s)" (to_string e1) (to_string e2)
  | Minus (e1, e2) -> Printf.sprintf "(%s - %s)" (to_string e1) (to_string e2)
  | Times (e1, e2) -> Printf.sprintf "(%s * %s)" (to_string e1) (to_string e2)
  | Neg e          -> Printf.sprintf "-%s" (to_string e)
  | Id id          -> Identifier.string_of id
  | Var id         -> Identifier.string_of id
