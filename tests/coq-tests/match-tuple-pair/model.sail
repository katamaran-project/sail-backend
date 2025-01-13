val fst : (int, int) -> int
function fst(x, y) = {
  let pair = (x, y) in
  match pair {
    (a, b) => a
  }
}

val snd : (int, int) -> int
function snd(x, y) = {
  let pair = (x, y) in
  match pair {
    (a, b) => b
  }
}