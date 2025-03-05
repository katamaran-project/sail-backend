include LoggingLib.Make(struct
    let verbosity_level () =
      Configuration.(get verbosity_level)
end)
