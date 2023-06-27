open Lib.Pretty_printing_katamaran
open Examples

(******************************************************************************)
(* Main *)

let () = 
  if Array.length Sys.argv = 1 || Array.length Sys.argv > 3 then
    print_endline "Usage: main.exe <input> [width]"
  else
    let width = if Array.length (Sys.argv) > 2
      then int_of_string Sys.argv.(2)
      else 80
    in let ir = match Sys.argv.(1) with 
      | "lists" -> ListsIR.ir
      | "long"  -> LongIR.ir
      | _       -> failwith "Unknown input"
    in pretty_print width Out_channel.stdout (fromIR_pp ir)