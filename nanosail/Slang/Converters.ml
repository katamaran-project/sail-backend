(*

   Module containing functionality related to converting Slang values into their
   corresponding OCaml values, e.g., Int n to n.

*)
open ExtBase
module EC = EvaluationContext
open Monads.Notations.Star(EC)
open Monads.OptionNotation


(*
   An 'a converter is a function that checks if a given Slang value
   can be converted into an OCaml value of type a.
   If so, it produces it as in a Some, otherwise a None is returned.
*)
type 'a converter = Value.t -> 'a option


(*
   A multiconverter converts multiple Slang values into a single OCaml value.
   Useful for dealing with function arguments that are expected to be of a certain type.
*)
type 'a multiconverter = Value.t list -> 'a option


let return x = Some x

let fail = None


(*
   Trivial converter.
   Used in situations where any value is welcome.
*)
let anything (v : Value.t) : Value.t option =
  Some v


let integer (value : Value.t) : int option =
  match value with
  | Value.Integer n -> return n
  | _               -> fail


let tuple2
    (f1    : 'a converter)
    (f2    : 'b converter)
    (value : Value.t     ) : ('a * 'b) option
  =
  match value with
  | Value.Cons (x1, Cons (x2, Nil)) -> begin
      let=? x1 = f1 x1
      and=? x2 = f2 x2
      in
      return (x1, x2)
    end
  | _                               -> fail


let tuple3
    (f1    : 'a converter)
    (f2    : 'b converter)
    (f3    : 'c converter)
    (value : Value.t     ) : ('a * 'b * 'c) option
  =
  match value with
  | Value.Cons (x1, Value.Cons (x2, Value.Cons (x3, Nil))) -> begin
      let=? x1 = f1 x1
      and=? x2 = f2 x2
      and=? x3 = f3 x3
      in
      return (x1, x2, x3)
    end
  | _                                                      -> fail


let string (value : Value.t) : string option =
  match value with
  | Value.String s -> return s
  | _              -> fail


let symbol (value : Value.t) : string option =
  match value with
  | Value.Symbol identifier -> return identifier
  | _                       -> fail


let cons
    (f     : 'a converter)
    (g     : 'b converter)
    (value : Value.t     ) : ('a * 'b) option
  =
  match value with
  | Value.Cons (car, cdr) -> begin
      let=? x = f car
      and=? y = g cdr
      in
      return (x, y)
    end
  | _                     -> fail


let nil (value : Value.t) : unit option =
  match value with
  | Value.Nil -> return ()
  | _         -> fail


let bool (value : Value.t) : bool option =
  match value with
  | Value.Bool x -> return x
  | _            -> fail


let truthy (value : Value.t) : bool option =
  return @@ Value.truthy value


let list
    ?(min_length : int = 0     )
    (f           : 'a converter)
    (value       : Value.t     ) : 'a list option
  =
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


let callable (value : Value.t) : Value.callable option =
  match value with
  | Value.Callable callable -> return callable
  | _                       -> fail


(*
   Expects values to contain exactly 1 item that satisfy the given pattern


   Example Usage
   -------------

     let fibonacci (arguments : Value.t list) =
       let=? n : int = map1 integer arguments
       in
       return @@ ...

*)
let map1
    (f      : 'a converter)
    (values : Value.t list) : 'a option
  =
  match values with
  | [v1] -> begin
      let=? v1' = f v1
      in
      return v1'
    end
  | _    -> fail


(*
   Expects values to contain exactly 2 items that satisfy the given patterns


   Example Usage
   -------------

     let xor(arguments : Value.t list) =
       let=? left_operand, right_operand = map2 bool bool arguments
       in
       return @@ ...

*)
let map2
    (f      : 'a converter)
    (g      : 'b converter)
    (values : Value.t list) : ('a * 'b) option
  =
  match values with
  | [v1; v2] -> begin
      let=? v1' = f v1
      and=? v2' = g v2
      in
      return (v1', v2')
    end
  | _        -> fail


(*
   Expects values to contain exactly 3 items that satisfy the given patterns

   Example Usage
   -------------

      let substring (arguments : Value.t list) =
         let=? string, start, stop = map3 string integer integer
         in
         return @@ ...
*)
let map3
    (f1     : 'a converter)
    (f2     : 'b converter)
    (f3     : 'c converter)
    (values : Value.t list) : ('a * 'b * 'c) option
  =
  match values with
  | [v1; v2; v3] -> begin
      let=? v1' = f1 v1
      and=? v2' = f2 v2
      and=? v3' = f3 v3
      in
      return (v1', v2', v3')
    end
  | _            -> fail


(*
   Expects values to contain exactly 4 items that satisfy the given patterns
*)
let map4
    (f1     : 'a converter)
    (f2     : 'b converter)
    (f3     : 'c converter)
    (f4     : 'd converter)
    (values : Value.t list) : ('a * 'b * 'c * 'd) option
  =
  match values with
  | [v1; v2; v3; v4] -> begin
      let=? v1' = f1 v1
      and=? v2' = f2 v2
      and=? v3' = f3 v3
      and=? v4' = f4 v4
      in
      return (v1', v2', v3', v4')
    end
  | _                -> fail
