val is_empty : list(int) -> bool
function is_empty(l) = match l {
  [||]   => true,
  _ :: _ => false
}
