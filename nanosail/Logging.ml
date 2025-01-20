open Base


let indentation_level = ref 0


let increase_indentation () =
  indentation_level := !indentation_level + 1


let decrease_indentation () =
  if
    Int.equal !indentation_level 0
  then
    failwith "cannot decrease indentation level when already at level 0"
  else
    indentation_level := !indentation_level - 1


let create_indentation_restorer () : unit -> unit =
  let current_indentation = !indentation_level
  in
  fun () -> indentation_level := current_indentation


let with_increased_indentation f =
  let restore_indentation = create_indentation_restorer ()
  in
  increase_indentation ();
  try
    let result = f ()
    in
    restore_indentation ();
    result
  with e -> begin
      restore_indentation ();
      raise e
    end


let verbosity_level_name (verbosity_level : int) : string =
  match verbosity_level with
  | 0 -> "NONE"
  | 1 -> "INFO"
  | 2 -> "DEBUG"
  | 3 -> "ERROR"
  | _ -> failwith "unknown verbosity level"


let log
    (level          : int            )
    (ocaml_position : Lexing.position)
    (message        : string lazy_t  ) : unit
  =
  if
    Configuration.(get verbosity_level) >= level
  then
    let filename    = ocaml_position.pos_fname
    and line_number = ocaml_position.pos_lnum
    and indentation = String.make !indentation_level ' '
    and level_name  = verbosity_level_name level
    in
    (* %! forces a flush *)
    ignore @@ Stdio.printf "%s[%s] (%s:%d) %s\n%!" indentation level_name filename line_number (Lazy.force message)


let info    = log 1
let warning = log 2
let debug   = log 3
let error   = log 4


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
    let result = with_increased_indentation f
    in
    exited_block_successfully ();
    result
  with e -> begin
      exited_block_with_exception ();
      raise e
    end
