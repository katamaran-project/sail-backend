exception TypeError

type 'a converter = Value.t -> 'a option

val (<|>)   : (Value.t -> 'a option) -> (Value.t -> 'a option) -> Value.t -> 'a option
val (|?>)   : 'a converter -> ('a -> 'b) -> 'b converter

val value   : Value.t converter
val integer : int converter
val tuple2  : 'a converter -> 'b converter -> ('a * 'b) converter
val tuple3  : 'a converter -> 'b converter -> 'c converter -> ('a * 'b * 'c) converter
val symbol  : string converter
val cons    : 'a converter -> 'b converter -> ('a * 'b) converter
val nil     : unit converter
val list    : 'a converter -> 'a list converter

val map     : 'a converter -> Value.t list -> 'a list option

module Notations : sig
  val (!!)    : 'a option -> 'a
  val (let=!) : 'a option -> ('a -> 'b) -> 'b
  val (and=!) : 'a option -> 'b option -> ('a * 'b) option
end
