default Order dec

$include <prelude.sail>

/*
  Patterns nested inside Some, with option translated to a sum.
  The cases for Some are merged into one case that binds the payload and matches it.
*/
enum Arch = { A32, A64, A128 }

/* A literal inside Some, then a catch-all (translationMode in CHERIoT). */
val is_32 : option(Arch) -> bool
function is_32(a) = match a {
  Some(A32) => true,
  _         => false,
}

/* Several literals, a binder for the rest and None. */
val width : option(Arch) -> int
function width(a) = match a {
  Some(A32) => 32,
  Some(A64) => 64,
  Some(x)   => 128,
  None()    => 0,
}

/* A tuple inside Some (step and dispatchInterrupt in CHERIoT). */
val sum_pair : option((int, bool)) -> int
function sum_pair(p) = match p {
  Some(n, b) => if b then n else 0,
  None()     => 0,
}

/* A tuple with a literal inside Some, a wildcard, and a catch-all that binds the whole value. */
val first_if_32 : option((Arch, int)) -> int
function first_if_32(p) = match p {
  Some(A32, n) => n,
  Some(_, _)   => 1,
  y            => match y { Some(_) => 2, None() => 3 },
}

/* Fall-through: a tuple case only partly covers Some; the binder case covers the rest. */
val fallthrough : option((Arch, bool)) -> bool
function fallthrough(p) = match p {
  Some(A64, true) => true,
  Some(q)         => { let (_, b) = q in not_bool(b) },
  None()          => false,
}

/* Another union translated to a sum: nested patterns on both sides. */
union verdict('a : Type, 'b : Type) = {
  Done   : 'a,
  Failed : 'b
}

val code : verdict(Arch, (Arch, int)) -> int
function code(o) = match o {
  Done(A32)        => 0,
  Failed(A64, n)   => n,
  Failed(_, _)     => 2,
  _                => 1,
}
