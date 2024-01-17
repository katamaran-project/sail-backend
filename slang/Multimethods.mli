exception DispatchFailure
exception ExecutionError

type 'a converter = Value.t -> 'a

val value   : Value.t converter
val integer : int converter
val string  : string converter
val tuple2  : 'a converter -> 'b converter -> ('a * 'b) converter
val tuple3  : 'a converter -> 'b converter -> 'c converter -> ('a * 'b * 'c) converter
val symbol  : string converter
val cons    : 'a converter -> 'b converter -> ('a * 'b) converter
val nil     : unit converter
val list    : 'a converter -> 'a list converter

val combine : ('a -> 'b) list -> 'a -> 'b

module Notations : sig
  val (<+>) : ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b
end
