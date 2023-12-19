open Nanosail.Gen.Katamaran
open Examples


let () =
  Nanosail.Configuration.(set use_list_notations true);
  pretty_print 80 Out_channel.stdout (fromIR_pp (find_ir "lists"))
