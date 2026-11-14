default Order dec

$include <prelude.sail>

/*
   (exclude-functions-as-no-op '("print_reg" "trace_write")) removes print_reg,
   an extern, and trace_write, a function, together with their calls: each call
   becomes the unit value. Without the exclusion, write_x1 calls two functions
   that machine.v does not define, and print_reg has no Rocq counterpart at all.
   CHERIoT excludes its printing and tracing this way.
*/

val print_reg = impure "print_reg" : string -> unit

register x1 : bits(8)

val trace_write : bits(8) -> unit
function trace_write(v) = print_reg("x1 <- value")

val write_x1 : bits(8) -> unit
function write_x1(v) = {
  x1 = v;
  trace_write(v);
  print_reg("written")
}

/* A call whose result is used: the unit value takes its place */
val write_and_return : bits(8) -> bits(8)
function write_and_return(v) = {
  let u : unit = trace_write(v);
  v
}
