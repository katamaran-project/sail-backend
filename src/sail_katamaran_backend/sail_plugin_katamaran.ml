open Libsail

let katamaran_target _ _ _ _ _ _ =
  print_string "ok.\n"

let _ = Target.register ~name:"katamaran" katamaran_target