default Order dec

$include <prelude.sail>


val foo : bitvector(2) <-> int
mapping foo = {
  0b00 <-> 0,
  0b01 <-> 1,
  0b10 <-> 2,
  0b11 <-> 3,
}
