module EC = Slang.EvaluationContext


let process
      (path        : string     )
      (_translation : Ast.program)
  =
  let open Slang in
  let open Monads.Notations.Star(Slang.EvaluationContext)
  in
  let contents = Stdio.In_channel.read_all path
  in
  let program =
    let* () = Prelude.initialize
    in
    (* let* () = *)
    (*   EC.iter !exported_functions ~f:(fun (id, callable) -> begin *)
    (*                                       EC.add_binding id (Slang.Value.Callable callable) *)
    (*                                     end) *)
    (* in *)
    let* _ = Evaluation.evaluate_string contents
    in
    EC.return ()
  in
  ignore @@ EvaluationContext.run program
