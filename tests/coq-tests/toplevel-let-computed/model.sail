default Order dec

$include <prelude.sail>


/* Top-level lets whose value is computed (calls, structs, struct updates, other top-level lets). */

enum Kind = { KData, KCap }

struct Cap = {
  tag     : bool,
  kind    : Kind,
  perms   : bits(4),
  address : bits(32)
}

type regtype = Cap

let addr_width = 32
let len_width = addr_width + 1
let max_addr : bits(32) = sail_ones(32)
let reset_perms : bits(4) = 0b1 @ sail_zeros(3)

let null_cap : Cap = struct {
  tag     = false,
  kind    = KData,
  perms   = sail_zeros(4),
  address = sail_zeros(32)
}

let root_cap : Cap = { null_cap with tag = true, perms = reset_perms, address = max_addr }
let zero_reg : regtype = null_cap
let default_kind : Kind = KCap


register reg : Cap


val reset : unit -> unit
function reset() = reg = root_cap


val is_null : Cap -> bool
function is_null(c) = c.tag == zero_reg.tag


val root_tag : unit -> bool
function root_tag() = root_cap.tag


val length : unit -> int
function length() = len_width + 1


val top : unit -> bits(32)
function top() = max_addr


val kind_of_default : unit -> Kind
function kind_of_default() = default_kind
