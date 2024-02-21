type    method_definition = Value.t list -> Value.t option EvaluationContext.t

val mk_multi_special_form  : method_definition list -> Value.callable
val mk_multimethod         : method_definition list -> Value.callable

