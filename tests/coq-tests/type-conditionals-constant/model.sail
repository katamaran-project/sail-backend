default Order dec
$include <prelude.sail>

/*
  Types that depend on a condition whose value is known, as when a configuration fixes a width
  (in a model, xlen would be `config base.xlen`, which --config instantiates):

    type physaddrbits_len : Int = if xlen == 32 then 34 else 64

  The plugin's rewrite fold_constant_type_conditionals replaces each such conditional by the
  branch its condition selects, so these widths translate as constants (ty.bvec 34).
*/
type xlen : Int = 32
type physaddrbits_len : Int = if xlen == 32 then 34 else 64
type physaddrbits = bits(physaddrbits_len)

type extension_enabled : Bool = false
type index_width : Int = if extension_enabled then 4 else 5

struct access = {
  address : physaddrbits,
  index   : bits(index_width),
}

register last_address : physaddrbits

val record_access : physaddrbits -> unit
function record_access(address) = last_address = address

val low_index : physaddrbits -> bits(index_width)
function low_index(address) = address[4 .. 0]

val make_access : physaddrbits -> access
function make_access(address) = struct { address = address, index = low_index(address) }
