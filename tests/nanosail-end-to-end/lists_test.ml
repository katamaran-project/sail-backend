open Nanosail.NanosailToMicrosail.Katamaran
open Examples

let () =
  output_document_to_channel 80 Out_channel.stdout (pretty_print (find_ir "lists"))
