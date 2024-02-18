type    method_definition = Value.t list -> Value.t option EvaluationContext.t

val mk_multi_special_form  : string -> method_definition list -> Value.callable
val mk_multimethod         : string -> method_definition list -> Value.callable
