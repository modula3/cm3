(* $Id$ *)

INTERFACE AdGridQ;
IMPORT Word, AdGridChild AS Child;
IMPORT LRPoint;

(* "adaptive grid quadrilateral"; this is the tile over which AdGrid.Ts are
   defined.  *)

TYPE 
  T = OBJECT METHODS
    corner(c : Child.T) : LRPoint.T;    

    (* in the following, adGrid is an AdGrid.T *)
    getLbound(adGrid : REFANY) : LONGREAL;
    getUbound(adGrid : REFANY) : LONGREAL;

    getCornerValues(adGrid : REFANY) : ARRAY Child.T OF LONGREAL;

    subdivide(levels : CARDINAL := 1) : REFANY; (* returns AdGridQSet.T 
                                                   of new tiles *)

    (* starting from at in m, find smallest neighboring tile in dir;
       NIL if no neighbor *)
    neighbor(at : LRPoint.T; dir : Dir) : T;

    allNeighborsAlongEdge(dir : Dir) : REFANY (*AdGridQSet.T*); 

    (* get linearly interpolated crossings where a level curve enters and
       leaves a given tile *)
    getCrossings(adGridP : REFANY; of : LONGREAL): ARRAY Dir OF REF LRPoint.T;
    
    (* is this tile larger in extent than "than"? *)
    larger(than : T) : BOOLEAN;

    (* absolute range of values across the corners of a tile *)
    range(adGridP : REFANY) : LONGREAL;
  END;

  Dir = { N, E, W, S };

CONST OppositeDir = ARRAY Dir OF Dir { Dir.S, Dir.W, Dir.E, Dir.N };

PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "AdGridQ";

END AdGridQ.
