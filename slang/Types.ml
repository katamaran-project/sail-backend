open! Base
open! Auxlib


type multimethod_error =
  | ArgumentTypeError
  | ExecutionError


exception MultimethodError of multimethod_error


module Result = Monads.Result.Make(struct type t = multimethod_error end)

open Result.Notations
open Monads.Notations.Star(Result)


type 'a converter = Value.t -> 'a Result.t


let value (v : Value.t) =
  Result.return v


let integer value =
  match value with
  | Value.Integer n -> Result.return n
  | _               -> Result.fail ArgumentTypeError


let tuple2 f1 f2 value =
  match value with
  | Value.Cons (x1, Cons (x2, Nil)) -> let* x1 = f1 x1 and* x2 = f2 x2 in Result.return (x1, x2)
  | _                               -> Result.fail ArgumentTypeError


let tuple3 f1 f2 f3 value =
  match value with
  | Value.Cons (x1, Value.Cons (x2, Value.Cons (x3, Nil))) -> let* x1 = f1 x1 and* x2 = f2 x2 and* x3 = f3 x3 in Result.return (x1, x2, x3)
  | _                                                      -> Result.fail ArgumentTypeError


let string value =
  match value with
  | Value.String s -> Result.return s
  | _              -> Result.fail ArgumentTypeError


let symbol value =
  match value with
  | Value.Symbol identifier -> Result.return identifier
  | _                       -> Result.fail ArgumentTypeError


let cons f g value =
  match value with
  | Value.Cons (car, cdr) -> let* car = f car and* cdr = g cdr in Result.return (car, cdr)
  | _                     -> Result.fail ArgumentTypeError


let nil value =
  match value with
  | Value.Nil -> Result.return ()
  | _         -> Result.fail ArgumentTypeError


let map f values =
  Result.all @@ List.map ~f values


(* TODO Removing 'value' leads to infinite loops, not sure why; may need to be investigated *)
let rec list f value =
  ((cons f (list f) |?> uncurry List.cons) <|> (nil |?> Fn.const [])) value

let binary_combine f g arg =
    match f arg with
    | Result.Success a                 -> Result.Success a
    | Result.Failure ArgumentTypeError -> g arg
    | Result.Failure ExecutionError    -> Result.Failure ExecutionError

let rec combine methods arg =
  match methods with
  | []    -> Result.Failure ArgumentTypeError
  | m::ms -> (binary_combine m (combine ms)) arg

module Notations = struct
  let (<+>) = binary_combine
end
