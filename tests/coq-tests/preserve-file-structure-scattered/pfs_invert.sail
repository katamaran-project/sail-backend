union clause ast = Invert : bits(8)

val invert : bits(8) -> bits(8)
function invert(x) = not_vec(x)

function clause execute(Invert(x)) = invert(x)
