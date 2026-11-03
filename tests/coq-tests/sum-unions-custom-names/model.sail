default Order dec

$include <prelude.sail>


// Constructors are declared in the opposite order of option's;
// the configuration decides which one goes left
union maybe('a : Type) = {
  Nothing : unit,
  Just    : 'a
}

val from_maybe : (int, maybe(int)) -> int
function from_maybe(default_value, m) = match m {
  Just(n)   => n,
  Nothing() => default_value
}

val just : bool -> maybe(bool)
function just(b) = Just(b)
