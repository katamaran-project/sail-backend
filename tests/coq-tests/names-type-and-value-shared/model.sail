default Order dec

$include <prelude.sail>

/*
  Sail has separate namespaces for types and values, Rocq has one.
  CHERIoT defines constants twice, as a type and as a value of the same name:
  the type keeps the name, the value's Rocq definition gets the suffix _value.
*/
type word_width : Int = 32

let word_width = sizeof(word_width)

type word = bits(word_width)

val get_width : unit -> int
function get_width() = word_width

val zero_word : unit -> word
function zero_word() = sail_zeros(word_width)
