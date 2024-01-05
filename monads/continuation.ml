type ('a, 'r) continuation =
  | Continuation of (('a -> 'r) -> 'r)

let run (Continuation f) return = f return

let return x =
  Continuation (fun r -> r x)

let bind
    (f :       ('a, 'r) continuation)
    (g : 'a -> ('b, 'r) continuation) : ('b, 'r) continuation =
  let Continuation f' = f
  in
  Continuation (fun retb ->
      f' (fun reta -> run (g reta) retb)
    )

let callcc (Continuation f) = f (fun r -> r)

let (let*) = bind

