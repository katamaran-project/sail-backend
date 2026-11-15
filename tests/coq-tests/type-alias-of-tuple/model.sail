default Order dec

$include <prelude.sail>

/*
  Type abbreviations of tuples become Rocq product types,
  e.g., CHERIoT's type ext_control_addr_error = (CapEx, capreg_idx).
*/
enum Err = { E1, E2 }

type err_and_index = (Err, bits(6))

type nested_tuple = (bool, (Err, bits(6)), bits(2))

val index_of : err_and_index -> bits(6)
function index_of(e) = {
  let (_, i) = e;
  i
}

val make : bits(6) -> err_and_index
function make(i) = (E2, i)
