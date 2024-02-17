open! Base
open! Auxlib
open Evaluation

module EC = EvaluationContext
open Monads.Notations.Star(EC)


type method_definition = Value.t list -> Value.t option EvaluationContext.t


let mk_multimacro methods arguments =
  let rec call_matching_method methods =
    match methods with
    | []    -> raise @@ Exception.SlangError "no method found"
    | m::ms -> begin
        let* state = EC.current_state
        in
        let* result = m arguments
        in
        match result with
        | Some result -> EC.return result
        | None        -> let* () = EC.set_current_state state in call_matching_method ms
      end
  in
  call_matching_method methods


let mk_multimethod (methods : method_definition list) arguments =
  let* evaluated_arguments = EC.map ~f:evaluate arguments
  in
  mk_multimacro methods evaluated_arguments
