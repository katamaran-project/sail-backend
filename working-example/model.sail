default Order dec

/* $include <vector_dec.sail> */
/* $include <regfp.sail> */

type xlen       : Int  = 32
type xlen_bytes : Int  = 4
type xlen_bits  : Int  = xlen_bytes * 8
type foo        : Int  = 3 * (8 + 1)
type bar        : Bool = true


/* /\* register RAX : bitvector(32) *\/ */
/* /\* register RBX : bitvector(32) *\/ */
/* /\* register RCX : int *\/ */
/* /\* register RDX : int *\/ */
/* /\* register REX : bitvector(32) *\/ */
/* /\* register RFX : bitvector(32) *\/ */
/* /\* register RGX : bitvector(32) *\/ */
/* /\* register RHX : bitvector(32) *\/ */
/* /\* register RIX : int *\/ */
/* /\* register RJX : int *\/ */
/* /\* register RKX : int *\/ */
/* /\* register RLONGNAME : bitvector(16) *\/ */

/* enum word_width = { BYTE, HALF, WORD, DOUBLE } */

/* mapping size_bits : word_width <-> bitvector(2) = { */
/*   BYTE   <-> 0b00, */
/*   HALF   <-> 0b01, */
/*   WORD   <-> 0b10, */
/*   DOUBLE <-> 0b11 */
/* } */


/* val is_empty : list(int) -> bool */
/* val empty : unit -> list(int) */
/* val onetwothree : unit -> list(int) */
/* val last : list(int) -> (int, bool) */
/* val listappend : (list(int), list(int)) -> list(int) */

/* function is_empty(l) = match l { */
/*   [||]   => true, */
/*   h :: t => false */
/* } */

/* function empty() = [||] */

/* function onetwothree() = [|1, 2, 3|] */

/* function last(l) = match l { */
/*   [||]   => (0, false), */
/*   h :: t => match t { */
/*     [||]     => (h, true), */
/*     h' :: t' => last(t) */
/*   } */
/* } */

/* function listappend(l1, l2) = match l1 { */
/*   [||]   => l2, */
/*   h :: t => h :: listappend(t, l2) */
/* } */

/* val switch_ii : (int, int) -> (int, int) */
/* function switch_ii(p) = match p { */
/*   (l, r) => (r, l) */
/* } */

/* val switch_ib : (int, bool) -> (bool, int) */
/* function switch_ib(p) = match p { */
/*   (l, r) => (r, l) */
/* } */

val test : unit -> unit
function test() = {
  ()
}

val test2: unit -> unit
function test2() = {
  test(); test()
}