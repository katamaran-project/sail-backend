class virtual ['a] setting = object
  method virtual get : 'a
  method virtual set : 'a -> unit
end


class ['a] variable_setting (initial : 'a) = object
  inherit ['a] setting
      
  val mutable value = initial

  method get = value

  method set (new_value : 'a) : unit =
    value <- new_value
end


type 'a t = 'a setting


let mk (initial : 'a) : 'a t =
  new variable_setting initial


let get (setting : 'a t) : 'a =
  setting#get


let set
    (setting : 'a t)
    (value   : 'a  ) : unit
  =
  setting#set value


let update
    (setting : 'a t)
    ~(f      : 'a -> 'a) : unit
  =
  setting#set (f setting#get)
