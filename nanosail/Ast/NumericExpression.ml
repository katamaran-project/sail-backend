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


let rec equal (t1 : t) (t2 : t) : bool =
  match t1, t2 with
   | Constant n1     , Constant n2      -> Z.equal n1 n2
   | Add (t1a, t1b)  , Add (t2a, t2b)   -> equal t1a t2a && equal t1b t2b
   | Minus (t1a, t1b), Minus (t2a, t2b) -> equal t1a t2a && equal t1b t2b
   | Times (t1a, t1b), Times (t2a, t2b) -> equal t1a t2a && equal t1b t2b
   | Neg t1          , Neg t2           -> equal t1 t2
   | Id id1          , Id id2           -> Identifier.equal id1 id2
   | Var id1         , Var id2          -> Identifier.equal id1 id2
   | _               , _                -> false
