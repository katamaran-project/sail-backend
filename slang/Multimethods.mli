type 'a converter         = Value.t -> 'a option
type 'a multiconverter    = Value.t list -> 'a option
type    method_definition = Value.t list -> Value.t option EvaluationContext.t

val value          : Value.t converter
val integer        : int converter
val string         : string converter
val symbol         : string converter

val cons           : 'a converter -> 'b converter -> ('a * 'b) converter
val nil            : unit converter

val list           : ?min_length : int -> 'a converter -> 'a list converter
val tuple2         : 'a converter -> 'b converter -> ('a * 'b) converter
val tuple3         : 'a converter -> 'b converter -> 'c converter -> ('a * 'b * 'c) converter

val map1           : 'a converter -> 'a multiconverter
val map2           : 'a converter -> 'b converter -> ('a * 'b) multiconverter
val map3           : 'a converter -> 'b converter -> 'c converter -> ('a * 'b * 'c) multiconverter
val mk_multimacro  : method_definition list -> Value.callable
val mk_multimethod : method_definition list -> Value.callable
