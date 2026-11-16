default Order dec
$include <prelude.sail>

enum CapEx = { CapEx_None, CapEx_TagViolation }
type capreg_idx = bits(6)

register last_error : capreg_idx

val handle : (CapEx, capreg_idx) -> unit
function handle(capEx, regnum) = match capEx { CapEx_None => (), CapEx_TagViolation => last_error = regnum }

// CHERIoT's ext_handle_data_check_error: a tuple pattern at a tuple type abbreviation
type ext_data_addr_error = (CapEx, capreg_idx)

function handle_error(err : ext_data_addr_error) -> unit = {
  let (capEx, regnum) = err;
  handle(capEx, regnum)
}
