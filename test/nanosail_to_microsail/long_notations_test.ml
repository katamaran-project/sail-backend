open Nanosail.Pp_katamaran
open Examples

let () =  
  opt_list_notations := true;
  pretty_print 80 Out_channel.stdout (fromIR_pp (find_ir "long"))
