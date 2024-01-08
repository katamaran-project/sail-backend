/*
val add_int = pure {interpreter: "add_int"} : (int, int) -> int

overload operator + = {add_int}
*/

/******************************************************************************/

val is_empty : list(int) -> bool
val empty : unit -> list(int)
val onetwothree : unit -> list(int)
val last : list(int) -> (int, bool)
val append : (list(int), list(int)) -> list(int)
/*
val length : list(int) -> int 
*/
val reverse_aux: (list(int), list(int)) -> list(int)
val reverse : list(int) -> list(int)
val reverse_bis : list(int) -> list(int)

/******************************************************************************/

function is_empty(l) = match l {
  [||]   => true,
  h :: t => false
}

function empty() = [||]

function onetwothree() = [|1, 2, 3|]

function last(l) = match l {
  [||]   => (0, false),
  h :: t => match t {
    [||]     => (h, true),
    h' :: t' => last(t)
  }
}

function append(l1, l2) = match l1 {
  [||]   => l2,
  h :: t => h :: append(t, l2) 
}

/*
function length(l) = match l {
  [||]   => 0,
  h :: t => length(t) + 1 
}
*/

function reverse_aux(l, acc) = match l {
  [||]   => acc,
  h :: t => reverse_aux(t, h :: acc)
}

function reverse(l) = reverse_aux(l, [||])

function reverse_bis(l) = match l {
  [||]   => [||],
  h :: t => append(reverse_bis(t), [|h|])
} 


