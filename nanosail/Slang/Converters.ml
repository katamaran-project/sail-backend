open ExtBase
module EC = EvaluationContext
open Monads.Notations.Star(EC)
open Monads.OptionNotation

type 'a converter         = Value.t -> 'a option
type 'a multiconverter    = Value.t list -> 'a option


let return x = Some x

let fail = None


let value (v : Value.t) =
  Some v


let integer value =
  match value with
  | Value.Integer n -> return n
  | _               -> fail


let tuple2 f1 f2 value =
  match value with
  | Value.Cons (x1, Cons (x2, Nil)) -> let=? x1 = f1 x1 and=? x2 = f2 x2 in return (x1, x2)
  | _                               -> fail


let tuple3 f1 f2 f3 value =
  match value with
  | Value.Cons (x1, Value.Cons (x2, Value.Cons (x3, Nil))) -> let=? x1 = f1 x1 and=? x2 = f2 x2 and=? x3 = f3 x3 in return (x1, x2, x3)
  | _                                                      -> fail


let string value =
  match value with
  | Value.String s -> return s
  | _              -> fail


let symbol value =
  match value with
  | Value.Symbol identifier -> return identifier
  | _                       -> fail


let cons f g value =
  match value with
  | Value.Cons (car, cdr) -> let=? x = f car and=? y = g cdr in return (x, y)
  | _                     -> fail


let nil value =
  match value with
  | Value.Nil -> return ()
  | _         -> fail


let bool value =
  match value with
  | Value.Bool x -> return x
  | _            -> fail


let truthy value =
  return @@ Value.truthy value


let list ?(min_length = 0) f value =
  let rec aux acc value =
    match value with
    | Value.Cons (car, cdr) -> let=? x = f car in aux (x :: acc) cdr
    | Value.Nil             -> return @@ List.rev acc
    | _                     -> fail
  in
  let=? elts = aux [] value
  in
  if List.length elts < min_length
  then fail
  else return elts


let callable value =
  match value with
  | Value.Callable callable -> return callable
  | _                       -> fail


(* expects values to contain exactly 1 item that satisfy the given pattern *)
let map1 (f : 'a converter) (values : Value.t list) =
  match values with
  | [v1] -> let=? v1' = f v1 in return v1'
  | _    -> fail


(* expects values to contain exactly 2 items that satisfy the given patterns *)
let map2 (f : 'a converter) (g : 'b converter) (values : Value.t list) =
  match values with
  | [v1; v2] -> let=? v1' = f v1 and=? v2' = g v2 in return (v1', v2')
  | _        -> fail


(* expects values to contain exactly 4 items that satisfy the given patterns *)
let map3 (f1 : 'a converter) (f2 : 'b converter) (f3 : 'c converter) (values : Value.t list) =
  match values with
  | [v1; v2; v3] -> let=? v1' = f1 v1 and=? v2' = f2 v2 and=? v3' = f3 v3 in return (v1', v2', v3')
  | _            -> fail


(* expects values to contain exactly 4 items that satisfy the given patterns *)
let map4 (f1 : 'a converter) (f2 : 'b converter) (f3 : 'c converter) (f4 : 'd converter) (values : Value.t list) =
  match values with
  | [v1; v2; v3; v4] -> let=? v1' = f1 v1 and=? v2' = f2 v2 and=? v3' = f3 v3 and=? v4' = f4 v4 in return (v1', v2', v3', v4')
  | _                -> fail
