module Function = struct
  let to_type (value_constructor : 'a -> Value.t) (id : string) (f : unit -> 'a) =
    let slang_function (arguments : Value.t list) =
      match arguments with
      | [] -> begin
          let result = f ()
          in
          EvaluationContext.return @@ value_constructor result
        end
      | _  -> failwith @@ Printf.sprintf "%s function does not expect arguments" id
    in
    Functions.mk_strict_function slang_function


  let to_string = to_type Value.Mk.string
  let to_bool   = to_type Value.Mk.bool
end
