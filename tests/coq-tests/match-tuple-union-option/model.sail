default Order dec

$include <prelude.sail>

/*
   A tuple match with a pattern nested inside a constructor (Read(Data)) or a sum's side
   (Some((v, m))) in a column: the column is matched first, one case per constructor, and the
   fields together with the other columns are then matched as a tuple (sail-riscv's phys_mem_read).
*/

enum Kind = { Data, Other }

union Access = {
  Read : Kind,
  Write : unit,
  Execute : unit,
}

val classify : (Access, option((bits(8), bool))) -> int
function classify(t, result) = match (t, result) {
  (Execute(_), None()) => 1,
  (Read(Data), None()) => 2,
  (_, None()) => 3,
  (_, Some((v, m))) => if m then unsigned(v) else 0,
}

// a match on a triple: its binders in order (tuplepat_snoc adds at the end)
val pick : (bits(8), bool, int) -> int
function pick(v, b, n) = match (v, b, n) {
  (x, true, k)  => unsigned(x) + k,
  (_, false, k) => k,
}
