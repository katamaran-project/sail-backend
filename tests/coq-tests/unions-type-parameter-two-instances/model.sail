default Order dec

$include <prelude.sail>

/*
  A union with a type parameter that translated code uses at two types.
  Each type gets its own monomorphic union, with suffixed union and constructor names.
*/
union Res('a : Type) = {
  Val : 'a,
  Exc : bits(4),
}

val make_bool : bool -> Res(bool)
function make_bool(b) = if b then Val(true) else Exc(0x1)

val make_bits : unit -> Res(bits(8))
function make_bits() = Val(0xFF)

val get_bits : Res(bits(8)) -> bits(8)
function get_bits(r) = match r {
  Val(v) => v,
  Exc(_) => 0x00,
}

val is_ok : Res(bool) -> bool
function is_ok(r) = match r {
  Val(b) => b,
  Exc(_) => false,
}

register last_result : Res(bool) = Exc(0x0)

val store : bool -> unit
function store(b) = last_result = make_bool(b)
