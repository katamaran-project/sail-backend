open Nanosail.NanosailToMicrosail.Katamaran
open Examples

let () =
  pretty_print 80 Out_channel.stdout (fromIR_pp (find_ir "long"))
