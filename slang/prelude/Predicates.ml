open Base
open Evaluation
open Multimethods
open Monads.Notations.Star(EvaluationContext)


module EV = EvaluationContext
module C = Converters
module P  = Value.Predicate

open Shared


let mk_predicate id pred args =
  let impl args =
    let=? v = C.(map1 value) args
    in
    EC.return @@ Some (Value.Bool (pred v))
  in
  mk_multimethod id [ impl ] args


let library env =
  EnvironmentBuilder.extend_environment env @@ fun { callable; _ } -> begin
      let predicate id func =
        callable id @@ mk_predicate id func
      in
      predicate "cons?"     P.is_cons;
      predicate "integer?"  P.is_integer;
      predicate "symbol?"   P.is_symbol;
      predicate "string?"   P.is_string;
      predicate "bool?"     P.is_bool;
      predicate "nil?"      P.is_nil;
      predicate "callable?" P.is_callable;
    end
