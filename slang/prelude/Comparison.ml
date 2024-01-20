open Base
open Evaluation
open EvaluationContext
open Monads.Notations.Star(EvaluationContext)

module M = Multimethods
module V = Value

open Shared


let equality_check args =
  let* evaluated_args = map evaluate args
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
  aux evaluated_args


let comparison converter comparator args =
  let=?? ns = List.map ~f:converter args
  in
  let result = Value.Bool (List.for_all ~f:(Auxlib.uncurry comparator) @@ Auxlib.consecutive_overlapping_pairs ns)
  in
  EC.return @@ Some result


let int_string_comparison int_comparator string_comparator args =
  M.mk_multimethod [
    comparison M.integer int_comparator;
    comparison M.string string_comparator;
  ] args


let less_than                = int_string_comparison (<)  String.(<)
let less_than_or_equal_to    = int_string_comparison (<=) String.(<=)
let greater_than             = int_string_comparison (>)  String.(>)
let greater_than_or_equal_to = int_string_comparison (>=) String.(>=)

let library env =
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      callable "="  equality_check;
      callable "<"  less_than;
      callable "<=" less_than_or_equal_to;
      callable ">"  greater_than;
      callable ">=" greater_than_or_equal_to;
    )
