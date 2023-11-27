type xlen       : Int = 32
type xlen_bytes : Int = 4
type xlen_bits  : Int = xlen_bytes * 8
type foo        : Int = 3 * (8 + 1)

register RAX : bitvector(32)
register RBX : bitvector(32)
register RCX : int
register RDX : int
register RLONGNAME : bitvector(16)

/*
val is_empty : list(int) -> bool
val empty : unit -> list(int)
val onetwothree : unit -> list(int)
val last : list(int) -> (int, bool)
val append : (list(int), list(int)) -> list(int)

function is_empty(l) = match l {
  [||]   => true,
  h :: t => false
}

function empty() = [||]

function onetwothree() = [|1, 2, 3|]

function last(l) = match l {
  [||]   => (0, false),
  h :: t => match t {
    [||]     => (h, true),
    h' :: t' => last(t)
  }
}

function append(l1, l2) = match l1 {
  [||]   => l2,
  h :: t => h :: append(t, l2)
}
*/