type 'a setting

val bool    : ?init:bool -> string -> bool setting
val strings :                   string -> string list setting

module Exported : sig
  val get                     : 'a setting -> 'a
  val set                     : 'a setting -> 'a -> unit

  val load_configuration_file : string -> unit
end
