default Order dec

$include <prelude.sail>

enum Direction = { North, South, East, West }

struct Point = {
  x : int,
  y : int
}

val step : (Point, Direction) -> Point
function step(p, d) = match d {
  North => struct { x = p.x,     y = p.y + 1 },
  South => struct { x = p.x,     y = p.y - 1 },
  East  => struct { x = p.x + 1, y = p.y     },
  West  => struct { x = p.x - 1, y = p.y     }
}