default Order dec

$include <prelude.sail>


union instruction = {
  Add : int,
  Sub : unit,
  Mul : (int, int)
}