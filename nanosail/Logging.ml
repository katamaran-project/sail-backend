include LogLib.Make(
  struct
    let verbosity_level () =
      Configuration.(get verbosity_level)

    let print = Stdio.print_endline
    
    let flush () =
      (* %! forces a flush *)
      Stdio.printf "%!"
  end
)
