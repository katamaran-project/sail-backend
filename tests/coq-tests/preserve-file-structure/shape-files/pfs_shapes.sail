// A union over a record of another file, and calls to functions of another file

union shape = {
  Dot : pixel,
  Line : (pixel, pixel)
}

register last : shape = Dot(struct { c = Red, v = 0x01 })

val first_pixel : shape -> pixel
function first_pixel(s) = {
  match s {
    Dot(p) => p,
    Line(p, _) => p
  }
}

val shape_brightness : shape -> bits(8)
function shape_brightness(s) = {
  let b = brightness(first_pixel(s));
  last = s;
  b
}

val clear : unit -> unit
function clear() = {
  current = struct { c = Blue, v = 0x00 }
}
