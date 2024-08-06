module Function = struct
  let to_type (value_constructor : 'a -> Value.t) (id : string) (f : unit -> 'a EvaluationContext.t) =
    let slang_function (arguments : Value.t list) =
      match arguments with
      | [] -> begin
          EvaluationContext.bind (f ()) @@ fun result -> EvaluationContext.return @@ value_constructor result
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
