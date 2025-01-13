val fst : (int, int) -> int
function fst(x) = {
  match x {
    (a, b) => a
  }
}

val snd : (int, int) -> int
function snd(x) = {
  match x {
    (a, b) => b
  }
}