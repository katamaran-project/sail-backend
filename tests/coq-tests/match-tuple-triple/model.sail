val fst : (int, int, int) -> int
function fst(x, y, z) = {
  let triple = (x, y, z) in
  match triple {
    (a, b, c) => a
  }
}

val snd : (int, int, int) -> int
function snd(x, y, z) = {
  let triple = (x, y, z) in
  match triple {
    (a, b, c) => b
  }
}

val third : (int, int, int) -> int
function third(x, y, z) = {
  let triple = (x, y, z) in
  match triple {
    (a, b, c) => c
  }
}
