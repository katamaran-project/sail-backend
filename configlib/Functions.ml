(*
  Creates a Slang function named <export_as> that takes a boolean argument.
  Calling this function causes a refcell to be set with this argument.
*)
let bool ?(init=false) export_as =
  let get, set = create_setting_cell init
  in
  let script_function _values =
    set true;
    EC.return Slang.Value.Nil
  in
  register_script_function export_as script_function;
  Setting (get, set)
