default Order dec

$include <prelude.sail>

/*
   Adding an integer literal to a bitvector whose type is a synonym (add_bits_int),
   as sail-riscv does with xlenbits:

       type xlen : Int = 32
       type xlenbits = bits(xlen)

   pmpAddrRange in sail-riscv's riscv_pmp_control.sail has no separate val
   declaration; its parameters are annotated inline, so they have the type
   xlenbits rather than bits(32):

       function pmpAddrRange(cfg: Pmpcfg_ent, pmpaddr: xlenbits, prev_pmpaddr: xlenbits) -> ... = {
         ...
         let mask = pmpaddr ^ (pmpaddr + 1);
*/

type xlen : Int = 32
type xlenbits = bits(xlen)

function napot_mask(pmpaddr : xlenbits) -> xlenbits = {
  xor_vec(pmpaddr, pmpaddr + 1)
}

val with_val_declaration : xlenbits -> xlenbits
function with_val_declaration(pmpaddr) = {
  pmpaddr + 4
}
