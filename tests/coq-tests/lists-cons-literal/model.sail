val empty : unit -> list(int)
function empty () = {
  return 1 :: (2 :: 3 :: [| |])
}