type 'a getter  = unit -> 'a
type 'a setter  = 'a -> unit
type 'a t       = Setting of 'a getter * 'a setter


let mk initial_value =
  let cell = ref initial_value
  in
  let get () = !cell
  and set b  = cell := b
  in
  Setting (get, set)

let get (Setting (getter, _    )) = getter ()
let set (Setting (_,     setter)) = setter
