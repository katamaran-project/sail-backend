open Monads.Notations.Star(EvaluationContext)


module Function = struct
  (*

     Given an OCaml function func with type
     
       unit -> 'a EC.t
     
     convert it to an equivalent Slang function that takes no parameters and returns
     the result of func () as a Slang value (e.g., OCaml strings are converted to Slang strings, etc.)


     value_constructor
       converts OCaml value to Slang value, see Value.Mk module
     id
       name of the function; used in error message
     func
       function running in evaluation context that returns OCaml value that
       needs converting
     
   *)
  let to_type
      (value_constructor : 'a -> Value.t                 )
      (id                : string                        )
      (func              : unit -> 'a EvaluationContext.t)
    =
    let slang_function (arguments : Value.t list) =
      match arguments with
      | [] -> begin
          let* result = func ()
          in
          EvaluationContext.return @@ value_constructor result
        end
      | _  -> failwith @@ Printf.sprintf "%s function does not expect arguments" id
    in
    Functions.mk_strict_function slang_function


  (* unit -> string *)
  let to_string = to_type Value.Mk.string

  (* unit -> bool *)
  let to_bool = to_type Value.Mk.bool

  (* unit -> unit *)
  let to_unit = to_type (fun () -> Value.Mk.nil)
end
