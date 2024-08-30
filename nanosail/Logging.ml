open Base


let log message =
  ignore @@ Stdio.printf "%s\n" message


let info message =
  log message


(* todo move to better place *)
let try_finally
    (f       : unit -> 'a  )
    (finally : unit -> unit) : 'a =
  try
    let result = f ()
    in
    finally ();
    result
  with e -> begin
      finally ();
      raise e
    end


let surround
    (logger  : string -> unit)
    (caption : string        )
    (f       : unit -> 'a    ) : 'a
  =
  let enter_block () =
    logger @@ Printf.sprintf " IN %s" caption;
  and exit_block () =
    logger @@ Printf.sprintf "OUT %s" caption;
  in
  enter_block ();
  try_finally f exit_block      

