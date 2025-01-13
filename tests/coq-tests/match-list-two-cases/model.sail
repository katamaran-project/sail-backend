default Order dec

$include <prelude.sail>

val use : list(int) -> int
function use _ = 0

val is_empty : list(int) -> int
function is_empty(l) = match l {
  [||]   => 0,
  h :: t => h + use(t)
}

val is_empty2 : list(int) -> int
function is_empty2(l) = match l {
  h :: t => h + use(t),
  [||]   => 0,
}

val is_empty3 : list(int) -> int
function is_empty3(l) = match l {
  _ :: t => use(t),
  [||]   => 0,
}

val is_empty4 : list(int) -> int
function is_empty4(l) = match l {
  h :: _ => h,
  [||]   => 0,
}

val is_empty5 : list(int) -> int
function is_empty5(l) = match l {
  _ :: _ => 0,
  [||]   => 0,
}
