open Base


let using ~resource:(x : 'a) ~close ~body =
  begin
    try
      body x
    with e ->
      close x;
      raise e
  end;
  close x
