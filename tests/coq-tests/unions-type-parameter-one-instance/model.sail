default Order dec

$include <prelude.sail>

/*
  A union with a type parameter that translated code uses at one type only.
  It becomes one monomorphic union that keeps Sail's names (Access, Read, ...).
  Kind is defined after Access, so the union can only be emitted after Kind.
*/
union Access('a : Type) = {
  Read      : 'a,
  Write     : 'a,
  ReadWrite : ('a, 'a),
  Execute   : unit,
}

enum Kind = { KData, KCap }

val kind_of : Access(Kind) -> Kind
function kind_of(a) = match a {
  Read(k)         => k,
  Write(k)        => k,
  ReadWrite(k, _) => k,
  Execute()       => KData,
}

val make_read : Kind -> Access(Kind)
function make_read(k) = Read(k)

val make_read_write : unit -> Access(Kind)
function make_read_write() = ReadWrite(KCap, KData)
