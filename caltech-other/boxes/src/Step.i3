(* $Id$ *)

INTERFACE Step;
IMPORT GridPoint, GridPointSet, GridPointCube;
IMPORT RouteState;
IMPORT ForbiddenSteps;

TYPE 
  T = RECORD 
    p : GridPoint.T; 
    cost : INTEGER;      (* cost of this ONE step *)
    type := Type.Normal; (* this has to do with ripping up *)
    s : RouteState.T;    (* output RouteState.T -- may involve growing bbox *)
  END;

  Type = { Normal, RipUp };

(* if res is NIL or too small, it will be extended ... *)
PROCEDURE Neighbors(READONLY p, 
                    dst : GridPoint.T; 
                    nonObstacles, pointsThatCostZero : GridPointSet.T; (* filled but not obstacks; points that can be stepped thru for free (can be NIL) *)
                    s : RouteState.T;
                    forbidden : ForbiddenSteps.T;
                    VAR res : Iterator;
                    READONLY absoluteMaxBBox : GridPointCube.T;
                    respectAbsMaxBBox : BOOLEAN);


TYPE
  Iterator <: PublicIter;

  PublicIter = OBJECT METHODS
    next(VAR s : T) : BOOLEAN
  END;
    
CONST Brand = "Step";

VAR DoSpecials : BOOLEAN; (* can change this to avoid testing rip-ups and cell grows *)

END Step.
