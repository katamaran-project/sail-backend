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
  | 1 -> [ AnsiColor.Decoration.BackgroundColor Red; AnsiColor.Decoration.ForegroundColor BrightWhite ]
  | 2 -> [ AnsiColor.Decoration.ForegroundColor AnsiColor.Color.Red ]
  | 3 -> [ AnsiColor.Decoration.ForegroundColor AnsiColor.Color.Green ]
  | _ -> [ ]

let to_message (verbose_level : t) : PP.document =
  PP.decorate (to_decoration verbose_level) (PP.string @@ to_string verbose_level)

let of_int (n : int) : t = n

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
