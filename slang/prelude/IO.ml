open Base
open Monads.Notations.Star(EvaluationContext)
open Multimethods

module EC = EvaluationContext
module C = Converters

open Shared


let print =
  let id = "print"
  and impl args =
    Stdio.printf "%s\n" @@ String.concat ~sep:" " @@ List.map ~f:Value.to_string args;
    EV.return @@ Option.some @@ Value.Nil
  in
  (id, mk_multimethod id [ impl ])



let library env =
  let definitions = [
    print;
  ]
  in
  EnvironmentBuilder.extend_environment env (fun { callable; _ } ->
      List.iter
        ~f:(Auxlib.uncurry callable)
        definitions
    )


let initialize =
  let definitions = [
    print;
  ]
  in
  let pairs =
    List.map ~f:(fun (id, c) -> (id, Value.Callable c)) definitions
  in
  EC.iter ~f:(Auxlib.uncurry EC.add_binding) pairs
