default Order dec
$include <prelude.sail>

enum color = { Red, Green }
struct pair = { a : bits(8), b : bool }

val pick : unit -> color
function pick() = { let c : color = undefined; c }

val bits8 : unit -> bits(8)
function bits8() = { let x : bits(8) = undefined; x }

val flag : unit -> bool
function flag() = { let b : bool = undefined; b }

val num : unit -> int
function num() = { let n : int = undefined; n }

val mkpair : unit -> pair
function mkpair() = { let p : pair = undefined; p }
