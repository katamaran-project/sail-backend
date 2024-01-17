type multimethod_error =
  | ArgumentTypeError
  | ExecutionError

exception MultimethodError of multimethod_error

module Result : Monads.Result.S with type error = multimethod_error

type 'a converter = Value.t -> 'a Result.t

val value   : Value.t converter
val integer : int converter
val string  : string converter
val tuple2  : 'a converter -> 'b converter -> ('a * 'b) converter
val tuple3  : 'a converter -> 'b converter -> 'c converter -> ('a * 'b * 'c) converter
val symbol  : string converter
val cons    : 'a converter -> 'b converter -> ('a * 'b) converter
val nil     : unit converter
val list    : 'a converter -> 'a list converter

val map     : ('a -> 'b Result.t) -> 'a list -> 'b list Result.t
val combine : ('a -> 'b Result.t) list -> 'a -> 'b Result.t

module Notations : sig
  val (<+>) : ('a -> 'b Result.t) -> ('a -> 'b Result.t) -> 'a -> 'b Result.t
end
