default Order dec

$include <prelude.sail>


val wrap : int -> option(int)
function wrap(x) = Some(x)

val nothing : unit -> option(bits(8))
function nothing() = None()

val wrap_bits : bits(8) -> option(bits(8))
function wrap_bits(x) = {
  let y : option(bits(8)) = Some(x);
  y
}
