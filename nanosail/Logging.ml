open! ExtBase


module Message = Document.WithoutAnnotations


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


module VerbosityLevel : sig
  type t

  val to_string   : t -> string
  val from_int    : int -> t
  val should_show : filter_level:t -> message_level:t -> bool

  val quiet       : t
  val error       : t
  val warning     : t
  val info        : t
  val debug       : t
  val default     : t
end = struct
  type t = int

  let to_string (verbosity_level : t) : string =
    match verbosity_level with
    | 0 -> "QUIET"
    | 1 -> "ERROR"
    | 2 -> "WARNING"
    | 3 -> "INFO"
    | 4 -> "DEBUG"
    | _ -> failwith "unknown verbosity level"

  let to_decoration (verbosity_level : t) : AnsiColor.Decoration.t list =
    match verbosity_level with
    | 1 -> [ AnsiColor.Decoration.BackgroundColor Red; AnsiColor.Decoration.ForegroundColor White ]
    | 2 -> [ AnsiColor.Decoration.ForegroundColor AnsiColor.Color.Red ]
    | 3 -> [ AnsiColor.Decoration.ForegroundColor AnsiColor.Color.Green ]
    | _ -> [ ]

  let to_message (verbose_level : t) : Message.t =
    Message.decorate (to_decoration verbose_level) (Message.string @@ to_string verbose_level)
  
  let from_int (n : int) : t = n

  let should_show
      ~(filter_level  : t)
      ~(message_level : t) : bool
    =
    filter_level >= message_level

  let quiet   = 0
  let error   = 1
  let warning = 2
  let info    = 3
  let debug   = 4
  let default = warning
end


let verbosity_level = ConfigLib.Setting.mk VerbosityLevel.default


let log
    (level          : VerbosityLevel.t)
    (ocaml_position : Lexing.position )
    (message        : Message.t lazy_t) : unit
  =
  if
    VerbosityLevel.should_show ~filter_level:Configuration.(get verbosity_level) ~message_level: level
  then
    let filename    = ocaml_position.pos_fname
    and line_number = ocaml_position.pos_lnum
    and level_name  = VerbosityLevel.to_string level
    in
    let output_message =
      let tag =
        Message.(enclose horizontal brackets (decorate (AnsiColor.Decoration.ForegroundColor level_color)
        Message.format "[%s] (%s:%d) " level_name filename line_number
      in
      Message.indent ~level:!indentation_level @@ Message.horizontal [tag; Lazy.force message]
    in
    let output_string =
      Message.to_string output_message
    in
    (* %! forces a flush *)
    Stdio.printf "%s\n%!" output_string


let error   = log VerbosityLevel.error
let warning = log VerbosityLevel.warning
let info    = log VerbosityLevel.info
let debug   = log VerbosityLevel.debug


let surround
    (ocaml_position : Lexing.position                            )
    (logger         : Lexing.position -> Message.t lazy_t -> unit)
    (caption        : string lazy_t                              )
    (f              : unit -> 'a                                 ) : 'a
  =
  let enter_block () =
    logger ocaml_position @@ lazy (Message.format "Entering %s" (Lazy.force caption))
  and exited_block_successfully () =
    logger ocaml_position @@ lazy (Message.format "Exiting %s" (Lazy.force caption))
  and exited_block_with_exception () =
    logger ocaml_position @@ lazy (Message.format "Escaping %s" (Lazy.force caption))
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
