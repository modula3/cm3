INTERFACE Compass;

TYPE
  Dir = { N, E, W, S };
  Set = SET OF Dir;
  
  (* a unit step in a compass direction is described: *)

  Incr = RECORD x, y : [-1..1]; END;

  (* and these are the steps that may be taken... *)
CONST
  HorizSet = Set { Dir.E, Dir.W };
  VertSet = Set { Dir.S, Dir.N };
  PosSet = Set { Dir.N, Dir.E };

  Step = ARRAY Dir OF Incr {
    Incr {  0,  1 },
    Incr {  1,  0 },
    Incr { -1,  0 },
    Incr {  0, -1 }
  };

  Reverse = ARRAY Dir OF Dir { Dir.S, Dir.W, Dir.E, Dir.N };
  Name = ARRAY Dir OF TEXT { "NORTH", "EAST", "WEST", "SOUTH" };
END Compass.
