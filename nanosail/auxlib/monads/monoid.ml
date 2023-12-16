module type S = sig
  type element

  val empty   : element
  val ( <+> ) : element -> element -> element
end
