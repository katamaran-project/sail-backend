open! ExtBase

module VerbosityLevel = VerbosityLevel


module type CONFIGURATION = sig
  val verbosity_level : unit -> VerbosityLevel.t
  val print           : string -> unit
  val flush           : unit -> unit
end


module Make(Configuration : CONFIGURATION) : sig
  val log     : VerbosityLevel.t -> Lexing.position -> PP.t lazy_t -> unit

  val error   : Lexing.position -> PP.t lazy_t -> unit
  val warning : Lexing.position -> PP.t lazy_t -> unit
  val info    : Lexing.position -> PP.t lazy_t -> unit
  val debug   : Lexing.position -> PP.t lazy_t -> unit
end = struct
  include Configuration
  
  let log
      (level          : VerbosityLevel.t  )
      (ocaml_position : Lexing.position   )
      (message        : PP.document lazy_t) : unit
    =
    if
      VerbosityLevel.should_show ~filter_level:(verbosity_level ()) ~message_level:level
    then
      let output_message =
        let level_message =
          PP.(enclose horizontal brackets (VerbosityLevel.to_message level))
        and location_message =
          let filename    = ocaml_position.pos_fname
          and line_number = ocaml_position.pos_lnum
          in
          PP.format "%s:%d" filename line_number
        in
        PP.horizontal [
          level_message;
          PP.space;
          PP.vertical [
            location_message;
            Lazy.force message;
          ]
        ]
      in
      let output : string =
        PP.to_string output_message
      in
      print output;
      flush ()


  let error   = log VerbosityLevel.error
  let warning = log VerbosityLevel.warning
  let info    = log VerbosityLevel.info
  let debug   = log VerbosityLevel.debug
end
