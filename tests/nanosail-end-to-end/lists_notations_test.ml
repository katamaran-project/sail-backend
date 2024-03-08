open Nanosail.NanosailToMicrosail.Katamaran
open Examples


let () =
  Nanosail.Configuration.(set use_list_notations true);
  output_document_to_channel 80 Out_channel.stdout (pretty_print (find_ir "lists"))
