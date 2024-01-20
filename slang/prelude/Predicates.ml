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
  EnvironmentBuilder.extend_environment env (fun { native_function; _ } ->
      native_function "cons?"    @@ mk_predicate P.is_cons;
      native_function "integer?" @@ mk_predicate P.is_integer;
      native_function "symbol?"  @@ mk_predicate P.is_symbol;
      native_function "string?"  @@ mk_predicate P.is_string;
      native_function "bool?"    @@ mk_predicate P.is_bool;
      native_function "nil?"     @@ mk_predicate P.is_nil;
      native_function "closure?" @@ mk_predicate P.is_closure;
      native_function "native?"  @@ mk_predicate P.is_native;
    )
