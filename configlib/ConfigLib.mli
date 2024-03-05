type 'a setting

val bool             : ?init:bool -> string -> bool setting
val strings          : string -> string list setting
val string_to_string : string -> (string * string) list setting
val callable         : ?error_message:string -> string -> (Slang.Value.t list -> Slang.Value.t Slang.EvaluationContext.t) setting

module Exported : sig
  val get                     : 'a setting -> 'a
  val set                     : 'a setting -> 'a -> unit

  val load_configuration_file : string -> unit
end
