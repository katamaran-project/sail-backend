let to_string (id : string) (f : unit -> string) =
  let slang_function (arguments : Value.t list) =
    match arguments with
    | [] -> begin
        let string = f ()
        in
        EvaluationContext.return @@ Value.Mk.string string
      end
    | _  -> failwith @@ Printf.sprintf "%s function does not expect arguments" id
  in
  Functions.mk_strict_function slang_function
