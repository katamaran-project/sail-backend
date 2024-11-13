val empty : unit -> list(int)
function empty() = [||]

val is_empty : unit -> bool
function is_empty() = {
  match [|0|] {
    [||]   => true,
    h :: t => false
  }
}
