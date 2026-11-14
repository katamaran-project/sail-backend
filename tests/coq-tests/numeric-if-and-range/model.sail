default Order dec

$include <prelude.sail>


/* Integer types whose numeric expression contains an if, and range types used as types. */

type Small = range(0, 1000)

struct Entry = {
  size  : Small,
  valid : bool
}

let max_size = 255

val get_size : Entry -> Small
function get_size(e) = e.size


/* The type of the if is int(if 'size > 255 then 255 else 'size) */
val clamped_size : Entry -> int
function clamped_size(e) = {
  let size = get_size(e);
  let clamped = if size > max_size then max_size else size;
  clamped + 1
}
