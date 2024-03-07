module BuildContext = BuildContext
module Setting      = Setting


(* let generic_strict *)
(*     ~init *)
(*     (translate : Slang.Value.t list -> 'a) *)
(*     (export_as : string                  ) *)
(*   = *)
(*   let get, set = create_setting_cell init *)
(*   in *)
(*   let script_function arguments = *)
(*     let open Slang in *)
(*     let open Slang.Prelude.Shared *)
(*     in *)
(*     let* evaluated_arguments = EC.map ~f:evaluate arguments *)
(*     in *)
(*     let strings = translate evaluated_arguments *)
(*     in *)
(*     set strings; *)
(*     EC.return @@ Value.Nil *)
(*   in *)
(*   register_script_function export_as script_function; *)
(*   Setting (get, set) *)
