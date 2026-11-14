default Order dec

$include <prelude.sail>


/* Sail allows different structs to have fields with the same name; Rocq record projections are global. */

struct Cap = {
  address : bits(32),
  tag     : bool
}


struct EncCap = {
  address  : bits(32),
  reserved : bool
}


struct Entry = {
  tag      : bool,
  reserved : bool
}


register cap   : Cap
register enc   : EncCap
register entry : Entry


val get_address : Cap -> bits(32)
function get_address(c) = c.address


val get_enc_address : EncCap -> bits(32)
function get_enc_address(e) = e.address


val copy_address : unit -> unit
function copy_address() = {
  enc.address = cap.address;
  cap.tag = entry.tag;
  entry.reserved = enc.reserved
}


val make_cap : (bits(32), bool) -> Cap
function make_cap(a, t) = struct { address = a, tag = t }
