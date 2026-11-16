default Order dec

$include <prelude.sail>

/*
   Sail's constraints on the size variables of a function become implicit hypotheses
   of its Fun constructor, next to the implicit widths:

     | low_half {kid__n : nat} {H : IsTrue (16 <=? Z.of_nat kid__n)%Z} : Fun [ "v" ∷ ty.bvec (kid__n) ] (ty.bvec (16))

   Rocq proves the hypothesis at each call (computed at concrete widths, by lia from the
   caller's hypothesis otherwise). A size variable that only the constraints relate to the
   types (kid__n of store_bytes) is a parameter too, passed by name. A branch in which Sail's
   flow typing fixes the size variables (width == 4) is translated at those values.
*/

register r16 : bits(16)
register r32 : bits(32)

// a slice that needs the constraint
val low_half : forall 'n, 'n >= 16. bits('n) -> bits(16)
function low_half(v) = v[15 .. 0]

// a call at a concrete width
val low_half_of_word : bits(32) -> bits(16)
function low_half_of_word(w) = low_half(w)

// a call that passes on the caller's own hypothesis
val low_half_twice : forall 'n, 'n >= 32. bits('n) -> bits(16)
function low_half_twice(v) = low_half(v)

// a size variable that only the constraints relate to the types
val store_bytes : forall 'n, 2 <= 'n <= 8. (int('n), bits(8 * 'n)) -> unit
function store_bytes(width, data) = r16 = data[15 .. 0]

val store_word : bits(32) -> unit
function store_word(w) = store_bytes(4, w)

// a branch that fixes the size variable (flow typing)
val store_flow : forall 'n, 0 < 'n <= 8. (int('n), bits(8 * 'n)) -> unit
function store_flow(width, data) =
  if width == 4 then r32 = data
  else if width == 2 then r16 = data
  else ()

val store_flow_word : bits(32) -> unit
function store_flow_word(w) = store_flow(4, w)

// a branch fact that fixes no value: the call needs 'n <= 8
val store_small : forall 'n, 0 < 'n <= 16. (int('n), bits(8 * 'n)) -> unit
function store_small(width, data) = if width <= 8 then store_flow(width, data) else ()

// a result type that Sail writes with a division (bits(8 * ('p8_times_n / 8)))
val read_bytes = pure "read_bytes" : forall 'n, 0 < 'n <= 8. int('n) -> bits(8 * 'n)

val read_wrapped : forall 'n, 0 < 'n <= 8. int('n) -> bits(8 * 'n)
function read_wrapped(width) = {
  let v = read_bytes(width);
  v
}

// a union at a flow-typed boolean: Res(bool('p)) is Res(bool)
union Res ('a : Type) = { Ok : 'a, Err : unit }

val flag_result : forall 'n, 'n > 0. int('n) -> Res(bool)
function flag_result(width) = {
  let r = Ok(width > 4);
  r
}

val int_result : unit -> Res(int)
function int_result() = Ok(1)
