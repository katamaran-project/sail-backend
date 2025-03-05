open! ExtBase

module VerbosityLevel = VerbosityLevel


module type CONFIGURATION = sig
  val verbosity_level : unit -> VerbosityLevel.t
end


module Make(Configuration : CONFIGURATION) = struct
  let log
      (level          : VerbosityLevel.t  )
      (ocaml_position : Lexing.position   )
      (message        : PP.document lazy_t) : unit
    =
    if
      VerbosityLevel.should_show ~filter_level:(Configuration.verbosity_level ()) ~message_level:level
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
      let output_string =
        PP.to_string output_message
      in
      (* %! forces a flush *)
      Stdio.printf "%s\n%!" output_string


  let error   = log VerbosityLevel.error
  let warning = log VerbosityLevel.warning
  let info    = log VerbosityLevel.info
  let debug   = log VerbosityLevel.debug


  let surround
      (ocaml_position : Lexing.position                              )
      (logger         : Lexing.position -> PP.document lazy_t -> unit)
      (caption        : string lazy_t                                )
      (f              : unit -> 'a                                   ) : 'a
    =
    let enter_block () =
      logger ocaml_position @@ lazy (PP.format "Entering %s" (Lazy.force caption))
    and exited_block_successfully () =
      logger ocaml_position @@ lazy (PP.format "Exiting %s" (Lazy.force caption))
    and exited_block_with_exception () =
      logger ocaml_position @@ lazy (PP.format "Escaping %s" (Lazy.force caption))
    in
    enter_block ();
    try
      let result = f ()
      in
      exited_block_successfully ();
      result
    with e -> begin
        exited_block_with_exception ();
        raise e
      end
end
