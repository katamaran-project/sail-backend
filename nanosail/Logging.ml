open Base


let log
    (level          : string         )
    (ocaml_position : Lexing.position)
    (message        : string lazy_t  ) : unit
  =
  let filename    = ocaml_position.pos_fname
  and line_number = ocaml_position.pos_lnum
  in
  ignore @@ Stdio.printf "[%s @ %s:%d] %s\n" level filename line_number (Lazy.force message)


let info    = log "INFO"
let warning = log "WARN"


let debug
    (ocaml_position : Lexing.position)
    (message        : string lazy_t  ) : unit
  =
  if Configuration.(get verbose)
  then log "DEBUG" ocaml_position message
  else ()


let surround
    (ocaml_position : Lexing.position                         )
    (logger         : Lexing.position -> string lazy_t -> unit)
    (caption        : string lazy_t                           )
    (f              : unit -> 'a                              ) : 'a
  =
  let enter_block () =
    logger ocaml_position @@ lazy (Printf.sprintf "Entering %s" (Lazy.force caption))
  and exited_block_successfully () =
    logger ocaml_position @@ lazy (Printf.sprintf "Exiting %s" (Lazy.force caption))
  and exited_block_with_exception () =
    logger ocaml_position @@ lazy (Printf.sprintf "Escaping %s" (Lazy.force caption))
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
