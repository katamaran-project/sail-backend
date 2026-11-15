default Order dec

$include <prelude.sail>


/*
   The constructors are not called zero and one: Sail's own library (vector.sail)
   binds local variables with those names, which an enum constructor would capture.
*/
enum my_bit = { Zero, One }

val flip : my_bit -> my_bit
function flip(x) = {
  match x {
    Zero => One,
    One  => Zero
  }
}
