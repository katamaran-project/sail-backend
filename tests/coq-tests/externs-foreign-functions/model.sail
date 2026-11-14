default Order dec

$include <prelude.sail>

/*
   Externs (val specifications with an extern binding and no Sail body) that do not
   correspond to a muSail operation become foreign functions: one constructor of FunX
   each, with the extern's signature, and every call a stm_foreign.

   * platform_ram_base and read_device have no known semantics: they are unconstrained.
     A call may return any value of its type (the value is universally quantified).
     platform_ram_base is pure, so a call leaves the memory unchanged; read_device and
     write_device are monadic, so a call may change the memory in any way.
   * popcount is polymorphic in the width of its argument: its FunX constructor takes
     the width as an implicit argument.
   * gcd_int is given its semantics in the configuration (Z.gcd).
*/

val platform_ram_base = pure {c: "plat_ram_base", _: "plat_ram_base"} : unit -> bits(32)
val read_device       = monadic {_: "read_device"} : bits(32) -> bits(32)
val write_device      = monadic {_: "write_device"} : (bits(32), bits(32)) -> unit
val popcount          = pure {_: "my_popcount"} : forall 'n, 'n >= 1. bits('n) -> int
val gcd_int           = pure {_: "my_gcd"} : (int, int) -> int

val load : bits(32) -> bits(32)
function load(offset) = read_device(add_bits(platform_ram_base(), offset))

val store : (bits(32), bits(32)) -> unit
function store(offset, value) = write_device(add_bits(platform_ram_base(), offset), value)

val ones_in_byte : bits(8) -> int
function ones_in_byte(b) = popcount(b)

val ones_in_word : bits(32) -> int
function ones_in_word(w) = popcount(w)

val gcd_with_12 : int -> int
function gcd_with_12(x) = gcd_int(12, x)
