val fst : (int, int) -> int
function fst(pair) = {
  match pair {
    (a, b) => a
  }
}

val snd : (int, int) -> int
function snd(pair) = {
  match pair {
    (a, b) => b
  }
}