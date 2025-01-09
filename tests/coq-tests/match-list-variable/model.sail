val empty : unit -> list(int)
function empty() = [||]

val is_empty : list(int) -> bool
function is_empty(l) = match l {
  [||]   => true,
  h :: t => false
}
