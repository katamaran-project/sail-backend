val is_empty : list(int) -> bool
function is_empty(l) = match l {
  [||]   => true,
  h :: t => false
}

val is_empty2 : list(int) -> bool
function is_empty2(l) = match l {
  h :: t => false,
  [||]   => true,
}

val is_empty3 : list(int) -> bool
function is_empty3(l) = match l {
  _ :: t => false,
  [||]   => true,
}

val is_empty4 : list(int) -> bool
function is_empty4(l) = match l {
  h :: _ => false,
  [||]   => true,
}

val is_empty5 : list(int) -> bool
function is_empty5(l) = match l {
  _ :: _ => false,
  [||]   => true,
}
