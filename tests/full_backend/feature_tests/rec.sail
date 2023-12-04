val and_list : list(bool) -> bool

function and_list(l) = match l {
  [||]   => true,
  t :: q => if t then and_list(q) else false
}