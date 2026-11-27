// Only types and constants: they go to base.v, and there is no Machine/pfs_types.v

enum colour = { Red, Green, Blue }

struct pixel = {
  c : colour,
  v : bits(8)
}

let max_value : bits(8) = 0xFF
