val let_test : unit -> bool

function let_test() =
  let a = true in
  let b = false in 
  let c = if b then a else b in 
  c