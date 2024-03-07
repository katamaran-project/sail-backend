type 'a getter  = unit -> 'a
type 'a setter  = 'a -> unit
type 'a t       = Setting of 'a getter * 'a setter


let create_setting_cell initial_value =
  let cell = ref initial_value
  in
  let get () = !cell
  and set b  = cell := b
  in
  (get, set)
