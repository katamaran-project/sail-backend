open Base


let (let*) x f = List.bind x ~f
let (and*) x y = let* x in let* y in List.return (x, y)
let return     = List.return
