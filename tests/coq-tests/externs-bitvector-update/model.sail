default Order dec

$include <prelude.sail>

/*
   bitvector_update(v, i, b) (binding vector_update) with an index known only at run time
   becomes a foreign call, with the default semantics semantics_bitvector_update:
   v with bit i set to b.
*/

val set_bit : forall 'i, 0 <= 'i < 8. (bits(8), int('i), bit) -> bits(8)
function set_bit(v, i, b) = bitvector_update(v, i, b)
