default Order dec

val add_atom = pure {ocaml: "add_int", interpreter: "add_int", lem: "integerAdd", c: "add_int", coq: "Z.add"}: forall 'n 'm.
  (int('n), int('m)) -> int('n + 'm)

val add_int = pure {ocaml: "add_int", interpreter: "add_int", lem: "integerAdd", c: "add_int", coq: "Z.add"}: (int, int) -> int

overload operator + = {add_atom, add_int}



val foo : forall 'n. int('n) -> int(2 * 'n)
function foo(x) = {
  x + x
}
