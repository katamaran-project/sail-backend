open Base
open ExtBase
open Monads.Notations.Star(EvaluationContext)


module EC = EvaluationContext
module C  = Converters
module P  = Value.Predicate

open Shared


let mk_predicate id pred =
  let impl args =
    let=? v = C.(map1 value) args
    in
    EC.return @@ Some (Value.Bool (pred v))
  in
  (id, Functions.mk_multimethod [ impl; error id ])


let is_cons     = mk_predicate "cons?" P.is_cons
let is_integer  = mk_predicate "integer?" P.is_integer
let is_symbol   = mk_predicate "symbol?" P.is_symbol
let is_string   = mk_predicate "string?" P.is_string
let is_bool     = mk_predicate "bool?" P.is_bool
let is_nil      = mk_predicate "nil?" P.is_nil
let is_callable = mk_predicate "callable?" P.is_callable


let initialize =
  let definitions = [
    is_cons;
    is_integer;
    is_symbol;
    is_string;
    is_bool;
    is_nil;
    is_callable;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Fn.uncurry EC.add_binding) pairs
