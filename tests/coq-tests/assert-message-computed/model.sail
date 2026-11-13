default Order dec

$include <prelude.sail>
$include <string.sail>

/* As in sail-riscv's prelude.sail */
overload operator ^ = {concat_str}

/*
   An assert whose message is computed at run time, shaped like internal_error
   in sail-riscv's riscv_types.sail (called from 32 places in the CHERIoT model):

       assert (false, file ^ ":" ^ dec_str(line) ^ ": " ^ s);

   Messages have no meaning for verification, so the translation may drop the
   computation of the message along with the message itself.
*/

val report : (string, int, string) -> unit
function report(file, line, s) = {
  assert(false, file ^ ":" ^ dec_str(line) ^ ": " ^ s)
}
