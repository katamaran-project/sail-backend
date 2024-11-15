val empty : unit -> list(int)
function empty () = {
  1 :: (2 :: 3 :: [| |])
}