open Base


let log message =
  ignore @@ Stdio.printf "%s\n" @@ Lazy.force message


let info message =
  log message


let debug message =
  log message


(* todo move to better place *)
(* let try_finally *)
(*     (f       : unit -> 'a  ) *)
(*     (finally : unit -> unit) : 'a = *)
(*   try *)
(*     let result = f () *)
(*     in *)
(*     finally (); *)
(*     result *)
(*   with e -> begin *)
(*       finally (); *)
(*       raise e *)
(*     end *)


let surround
    (logger   : string lazy_t -> unit )
    (position : Lexing.position       )
    (caption  : string                )
    (f        : unit -> 'a            ) : 'a
  =
  let enter_block () =
    logger @@ lazy (Printf.sprintf " IN %s (%s)" caption (StringOf.OCaml.position position))
  and exited_block_successfully () =
    logger @@ lazy (Printf.sprintf "OUT %s" caption)
  and exited_block_with_exception () =
    logger @@ lazy (Printf.sprintf "XXX %s" caption)
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
