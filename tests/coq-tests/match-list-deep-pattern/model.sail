val length : list(int) -> int
function length(l) = match l {
  [||]   => 0,
  [| _ |] => 1,
  [| _, _ |] => 2,
  [| _, _, _ |] => 3,
  [| _, _, _, _ |] => 4,
  _ => 5
}
