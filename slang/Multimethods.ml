open! Base
open! Auxlib
open Evaluation

module EC = EvaluationContext
open Monads.Notations.Star(EC)


type method_definition = Value.t list -> Value.t option EvaluationContext.t


(*
   A special form receives its arguments in unevaluated form, i.e.

     (special-form (+ 1 2))

   receives the list (+ 1 2) as argument, not 3.
   A special form is not a macro: the result is considered the end result, whereas
   a macro will evaluate it
 *)
let mk_multi_special_form methods arguments =
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


(*
   A multimethod is a function that receives its arguments after they have been evaluated,
   i.e., strict evaluation.
 *)
let mk_multimethod (methods : method_definition list) arguments =
  let* evaluated_arguments = EC.map ~f:evaluate arguments
  in
  mk_multi_special_form methods evaluated_arguments
