val is_empty : list(int) -> bool
function is_empty(l) = match l {
  [||]   => true,
  h :: t => false
}

val is_empty' : list(int) -> bool
function is_empty'(l) = match l {
  h :: t => false,
  [||]   => true
}

val switch : (int, bool) -> (bool, int)
function switch(p) = match p {
  (l, r) => (r, l)
}

