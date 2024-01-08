module Star (M : Sig.Monad) = struct
  let (let*) = M.bind

  let (and*) x y = M.bind x (fun x' -> M.bind y (fun y' -> M.return (x', y')))
end


module Plus (M : Sig.Monad) = struct
  let (let+) = M.bind

  let (and+) x y = M.bind x (fun x' -> M.bind y (fun y' -> M.return (x', y')))
end


module Minus (M : Sig.Monad) = struct
  let (let-) = M.bind

  let (and-) x y = M.bind x (fun x' -> M.bind y (fun y' -> M.return (x', y')))
end
