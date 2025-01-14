val fst : (int, int, int) -> int
function fst(triple) = {
  match triple {
    (a, b, c) => a
  }
}

val snd : (int, int, int) -> int
function snd(triple) = {
  match triple {
    (a, b, c) => b
  }
}

val third : (int, int, int) -> int
function third(triple) = {
  match triple {
    (a, b, c) => c
  }
}
