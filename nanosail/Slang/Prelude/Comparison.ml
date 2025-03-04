open! ExtBase
open Evaluation
open EvaluationContext
open Monads.Notations.Star(EvaluationContext)

module V = Value
module C = Converters

open Shared


let equality_check =
  let id = "="
  in
  let impl arguments =
    let* evaluated_arguments = evaluate_sequentially arguments
    in
    let rec aux values =
      match values with
      | []       -> return @@ V.Bool true
      | [_]      -> return @@ V.Bool true
      | x::y::xs ->
        if not (V.equal x y)
        then return @@ V.Bool false
        else aux @@ y::xs
    in
    aux evaluated_arguments
  in
  (id, impl)


let comparison converter comparator =
  let impl args =
    let=?? ns = List.map ~f:converter args
    in
    let result = Value.Bool (List.for_all ~f:(Fn.uncurry comparator) @@ List.consecutive_overlapping_pairs ns)
    in
    EC.return @@ Some result
  in
  impl


let int_string_comparison id int_comparator string_comparator =
  let mm = Functions.mk_multimethod [
      comparison C.integer int_comparator;
      comparison C.string  string_comparator;
      error id;
    ]
  in
  (id, mm)


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
