open Base
open Value


let (let*) x f = Option.bind x ~f

let (and*) = Option.both

let return = Option.return

type 'a converter = Value.t -> 'a option


exception TypeError


let (<|>) f g =
  let converter value =
    match f value with
    | Some x -> Some x
    | None   -> begin
        match g value with
        | Some x -> Some x
        | None   -> None
      end
  in
  converter

let (|?>) x f = Option.map x ~f


let value v =
  Some v


let integer value =
  match value with
  | Integer n -> Some n
  | _         -> None


let tuple2 f1 f2 value =
  match value with
  | Cons (x1, Cons (x2, Nil)) -> let* x1 = f1 x1 and* x2 = f2 x2 in return (x1, x2)
  | _                         -> None


let tuple3 f1 f2 f3 value =
  match value with
  | Cons (x1, Cons (x2, Cons (x3, Nil))) -> let* x1 = f1 x1 and* x2 = f2 x2 and* x3 = f3 x3 in return (x1, x2, x3)
  | _                                    -> None


let symbol value =
  match value with
  | Symbol identifier -> Some identifier
  | _                 -> None


let cons f g value =
  match value with
  | Cons (car, cdr) -> let* car = f car and* cdr = g cdr in return (car, cdr)
  | _               -> None


let nil value =
  match value with
  | Nil -> Some ()
  | _   -> None


let map f values =
  Option.all @@ List.map ~f values


let list f value =
  match value with
  | Cons (_, _) -> map f @@ cons_to_list value
  | Nil         -> return @@ []
  | _           -> None
  (* (cons List.cons f (list f)) <|> (nil []) *)


let (!!) r =
  match r with
  | Some r -> r
  | None   -> raise TypeError


let (let=!) x f =
  match x with
  | Some r -> f r
  | None   -> raise TypeError

let (and=!) x y =
  let=! x = x
  in
  let=! y = y
  in
  return (x, y)
