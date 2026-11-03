default Order dec

$include <prelude.sail>


struct Entry = {
  key   : int,
  value : option(bits(8))
}

val make_entry : int -> Entry
function make_entry(k) = struct { key = k, value = None() }

val entry_value : Entry -> option(bits(8))
function entry_value(e) = e.value
