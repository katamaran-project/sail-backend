default Order dec

$include <prelude.sail>

/*
   A struct with a numeric parameter is one Rocq record family indexed by it:

     Record S (kid__m : nat) : Set := MkS { S_value : bv (8 * kid__m); S_tag : bool }.
     | Rs (kid__m : nat)                                     (in Records)

   Its uses at closed widths (S(4)) are the family at that width.
*/

type bytes('n) = bits(8 * 'n)

struct S('m) = {
  value : bytes('m),
  tag   : bool,
}

register last : S(2) = struct { value = 0x0000, tag = false }

val get_tag : forall 'n, 0 < 'n <= 8. S('n) -> bool
function get_tag(s) = s.tag

val clear_tag : forall 'n, 0 < 'n <= 8. S('n) -> S('n)
function clear_tag(s) = { s with tag = false }

val make_word : bits(32) -> S(4)
function make_word(v) = struct { value = v, tag = false }

val word_value : S(4) -> bits(32)
function word_value(s) = s.value

val tagged_word : bits(32) -> bool
function tagged_word(v) = get_tag(clear_tag(make_word(v)))

val save : bits(16) -> unit
function save(v) = last = struct { value = v, tag = true }

val load : unit -> bits(16)
function load() = last.value

/* A field whose width is the parameter itself, at an open width */

struct W('n) = {
  bits : bits('n),
  ok   : bool,
}

val wrap : forall 'n, 'n > 0. bits('n) -> W('n)
function wrap(b) = struct { bits = b, ok = true }

val unwrap : forall 'n, 'n > 0. W('n) -> bits('n)
function unwrap(w) = w.bits

val wrap_byte : bits(8) -> bits(8)
function wrap_byte(b) = unwrap(wrap(b))
