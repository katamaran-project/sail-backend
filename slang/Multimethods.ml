open! Base
open! Auxlib


exception DispatchFailure
exception ExecutionError


type 'a converter = Value.t -> 'a


let value (v : Value.t) =
  v


let integer value =
  match value with
  | Value.Integer n -> n
  | _               -> raise DispatchFailure


let tuple2 f1 f2 value =
  match value with
  | Value.Cons (x1, Cons (x2, Nil)) -> let x1 = f1 x1 and x2 = f2 x2 in (x1, x2)
  | _                               -> raise DispatchFailure


let tuple3 f1 f2 f3 value =
  match value with
  | Value.Cons (x1, Value.Cons (x2, Value.Cons (x3, Nil))) -> (f1 x1, f2 x2, f3 x3)
  | _                                                      -> raise DispatchFailure


let string value =
  match value with
  | Value.String s -> s
  | _              -> raise DispatchFailure


let symbol value =
  match value with
  | Value.Symbol identifier -> identifier
  | _                       -> raise DispatchFailure


let cons f g value =
  match value with
  | Value.Cons (car, cdr) -> (f car, g cdr)
  | _                     -> raise DispatchFailure


let nil value =
  match value with
  | Value.Nil -> ()
  | _         -> raise DispatchFailure


let rec list f value =
  match value with
  | Value.Cons (car, cdr) -> f car :: list f cdr
  | Value.Nil             -> []
  | _                     -> raise DispatchFailure


let binary_combine f g arg =
  try
    f arg
  with
  | DispatchFailure -> g arg


let rec combine methods arg =
  match methods with
  | []    -> raise DispatchFailure
  | m::ms -> (binary_combine m (combine ms)) arg


module Notations = struct
  let (<+>) = binary_combine
end
