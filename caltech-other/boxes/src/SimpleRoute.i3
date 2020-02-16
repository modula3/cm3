(* $Id$ *)

INTERFACE SimpleRoute;
IMPORT GridPointSet, GridPointSeq;
IMPORT GridPointCube;
IMPORT Route;
FROM Route IMPORT NotFound;

TYPE 
  T <: Public;

  Public = Route.T OBJECT METHODS
    computeRouteBetweenSets(
              (* route between from and to *)
              fromSet, toSet : GridPointSet.T;  

              (* treating the following as non-obstacles
                 (should include reachable layout in from and to) *)
              nonObstacles : GridPointSet.T;    

              multiplier : REAL;

              READONLY BBox : GridPointCube.T;
              (* bbox w/in which to route *)

              respectBBox : BOOLEAN;
              (* stay inside bbox on THIS route *)
              
              keepOuts : GridPointSet.T := NIL;
              (* the route will stay out of the keepOuts, guaranteed 
                 keepOuts must not overlap fromSet, toSet, or nonObstacles

                 keepOuts need ONLY implement the member() method.

                 if keepOuts is NIL, then it will be treated as the empty
                 set
              *)

              VAR velocity : Velocity (* for stats *);
              ripupSeq : GridPointSeq.T (* seq. of ripups *)
    ) RAISES { NotFound };
  END;

TYPE
  Velocity = RECORD
    v := 1.0d0;
    iter := 0;
  END;

CONST Brand = "Simple Gridpoint Router Route";

PROCEDURE SetRipUpFilename(fn : TEXT);

END SimpleRoute.


