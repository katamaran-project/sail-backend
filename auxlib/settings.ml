type 'a getter = unit -> 'a

type 'a setter = 'a -> unit

type 'a t = Setting of 'a getter * 'a setter

let create (default : 'a) : 'a t =
  let cell = ref default
  in
  let getter () = !cell
  and setter x  = cell := x
  in
  Setting (getter, setter)

let get (Setting (getter, _)) =
  getter ()

let set (Setting (_, setter)) x =
  setter x
