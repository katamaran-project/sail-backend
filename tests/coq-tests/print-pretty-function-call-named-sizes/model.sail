default Order dec

$include <prelude.sail>

/*
   A size variable that only the constraints relate to the types (kid__n of store_bytes) is
   passed by name: the callee is (store_bytes (kid__n := 4)). Katamaran's call notation only
   accepts a name as callee, so with pretty-print-function-calls such a call is still printed
   with stm_call; the call to a callee without named arguments (store_word) uses the notation.
*/

register r16 : bits(16)

val store_bytes : forall 'n, 2 <= 'n <= 8. (int('n), bits(8 * 'n)) -> unit
function store_bytes(width, data) = r16 = data[15 .. 0]

val store_word : bits(32) -> unit
function store_word(w) = store_bytes(4, w)

val store_word_twice : bits(32) -> unit
function store_word_twice(w) = {
  store_word(w);
  store_word(w)
}
