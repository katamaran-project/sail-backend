default Order dec

$include <prelude.sail>

// With preserve-file-structure, the functions of each included file go to Machine/<file>.v,
// keeping its directory (sanitised): those of shape-files/pfs_shapes.sail go to Machine/shape_files/pfs_shapes.v
$include "pfs_types.sail"
$include "pfs_colours.sail"
$include "shape-files/pfs_shapes.sail"


// A function of model.sail itself, which calls functions of both included files
val main : unit -> bits(8)
function main() = {
  clear();
  shape_brightness(Dot(current))
}
