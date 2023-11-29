open Nanosail.Pp_katamaran
open Examples

let () =  
  pretty_print 80 Out_channel.stdout (fromIR_pp (find_ir "prod"))
