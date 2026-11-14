default Order dec

$include <prelude.sail>

/*
  Assignments to a field of a bitfield register, r->F() = v and r[F] = v.
  Sail turns r->F() = v into a call of the setter _set_Status_IE(ref status, v);
  muSail has no register references, so the call is inlined:
  the register is read, updated with _update_Status_IE, and written back.
  The setters themselves are not translated.
*/
bitfield Status : bits(8) = {
  IE  : 3,
  PRV : 1 .. 0,
}

register status : Status

val disable : unit -> unit
function disable() = {
  status->IE() = 0b0
}

val set_privilege : bits(2) -> unit
function set_privilege(p) = {
  status->PRV() = p;
  status[IE] = 0b1
}

val read_ie : unit -> bits(1)
function read_ie() = status[IE]
