open Base
open Evaluation
open Monads.Notations.Star(EvaluationContext)


module EV = EvaluationContext
module M  = Multimethods
module P  = Value.Predicate

open Shared


let mk_predicate pred args =
  let impl args =
    let=? v = M.(map1 value) args
    in
    EC.return @@ Some (Value.Bool (pred v))
  in
  M.mk_multimethod [ impl ] args


let library env =
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      callable "cons?"     @@ mk_predicate P.is_cons;
      callable "integer?"  @@ mk_predicate P.is_integer;
      callable "symbol?"   @@ mk_predicate P.is_symbol;
      callable "string?"   @@ mk_predicate P.is_string;
      callable "bool?"     @@ mk_predicate P.is_bool;
      callable "nil?"      @@ mk_predicate P.is_nil;
      callable "callable?" @@ mk_predicate P.is_callable;
    )
