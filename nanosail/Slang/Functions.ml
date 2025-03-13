open Evaluation

module EC = EvaluationContext
open Monads.Notations.Star(EC)


let mk_strict_function f =
  let wrapper arguments =
    let* evaluated_arguments = EC.map ~f:evaluate arguments
    in
    f evaluated_arguments
  in
  Value.Mk.callable wrapper


type method_definition = Value.t list -> Value.t option EvaluationContext.t


(*
   A special form receives its arguments in unevaluated form, i.e.

     (special-form (+ 1 2))

   receives the list (+ 1 2) as argument, not 3.
   A special form is not a macro: the result is considered the end result, whereas
   a macro will evaluate proceed to evaluate it.
 *)
let mk_multi_special_form
    (methods   : method_definition list)
    (arguments : Value.t list          ) : Value.t EC.t
  =
  let rec call_matching_method methods =
    match methods with
    | []    -> raise @@ Exception.SlangError "no method found"
    | m::ms -> begin
        let* original_state = EC.(get State.state)
        in
        let* result = m arguments
        in
        match result with
        | Some result -> EC.return result
        | None -> begin
            let* () = EC.(put State.state original_state)
            in
            call_matching_method ms
          end
      end
  in
  call_matching_method methods


(*
   A multimethod is built out of multiple definitions, each with their own precondition on the arguments.
   When the precondition is not satisfied, it must signal this by returning None so that the next definition
   can be tried out.
   It is an error if all definitions reject the given arguments.

   An example usage of multimethods is +:
   one definition will specialize in a list of integers
   (and therefore have a precondition demanding that all arguments be of type Integer),
   another definition will deal with the concatenation of strings
   (and demands that all its arguments be strings).

   A multimethod is a function that receives its arguments after they have been evaluated, i.e., strict evaluation.
*)
let mk_multimethod
    (methods   : method_definition list)
    (arguments : Value.t list          ) : Value.t EC.t
  =
  let* evaluated_arguments = EC.map ~f:evaluate arguments
  in
  mk_multi_special_form methods evaluated_arguments
