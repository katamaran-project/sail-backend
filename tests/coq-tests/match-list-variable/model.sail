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
