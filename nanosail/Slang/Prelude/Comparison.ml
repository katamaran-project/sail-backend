open! ExtBase
open Evaluation
open Monads.Notations.Star(EvaluationContext)

module V = Value
module C = Converters

open Shared


(*
   (= arg1 arg2 ... argn)

   Checks if all arguments are equal.
   (=) returns true.
   (= arg) returns true.
*)
let equality_check =
  let id = "="
  in
  let impl arguments =
    let* evaluated_arguments = evaluate_sequentially arguments
    in
    let rec aux values =
      match values with
      | []       -> EC.return @@ V.Bool true
      | [_]      -> EC.return @@ V.Bool true
      | x::y::xs -> begin
          if
            not (V.equal x y)
          then
            EC.return @@ V.Bool false
          else
            aux @@ y::xs
        end
    in
    aux evaluated_arguments
  in
  (id, impl)


(*
   Returns a function that, given a list, first converts all items and
   then compares adjacent pairs using comparator.

   If a conversion fails, the function returns None.
   If all comparisons succeed, the function returns the Slang value for true, otherwise false.
*)
let comparison
    (converter  : 'a -> 'b option )
    (comparator : 'b -> 'b -> bool) : 'a list -> V.t option EC.t
  =
  let impl (args : 'a list) : V.t option EC.t =
    let=?? ns = List.map ~f:converter args
    in
    let result = Value.Bool (List.for_all ~f:(Fn.uncurry comparator) @@ List.consecutive_overlapping_pairs ns)
    in
    return result
  in
  impl


(*
   Helper function to create overloads for comparison operators.
*)
let int_string_comparison
    (identifier        : string                  )
    (int_comparator    : int -> int -> bool      )
    (string_comparator : string -> string -> bool)
  =
  let multimethod = Functions.mk_multimethod [
      comparison C.integer int_comparator;
      comparison C.string  string_comparator;
      error identifier;
    ]
  in
  (identifier, multimethod)


let less_than                = int_string_comparison "<"  (<)  String.(<)
let less_than_or_equal_to    = int_string_comparison "<=" (<=) String.(<=)
let greater_than             = int_string_comparison ">"  (>)  String.(>)
let greater_than_or_equal_to = int_string_comparison ">=" (>=) String.(>=)


let initialize =
  let definitions = [
    equality_check;
    less_than;
    less_than_or_equal_to;
    greater_than;
    greater_than_or_equal_to;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
