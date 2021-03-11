(* $Id$ *)

INTERFACE EndPointStatus;
IMPORT GridPoint;

TYPE 
  Dir = { N, E, W, S, U, D };

  T = ARRAY Dir OF BOOLEAN;
  Step = RECORD dx, dy, dl : INTEGER END;

CONST
  DirName = ARRAY Dir OF TEXT { "N", "E", "W", "S", "U", "D" };
  
  TopOnly = T { FALSE, FALSE, FALSE, FALSE, TRUE, FALSE };

  DirStep = ARRAY Dir OF Step {
    Step {  0,  1,  0 },
    Step {  1,  0,  0 },
    Step { -1,  0,  0 },
    Step {  0, -1,  0 },
    Step {  0,  0,  1 },
    Step {  0,  0, -1 } };

PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "EndPointStatus";

(* direction in which we must leave/enter endpoint *)
(* direction is relative to endpoint, i.e., E means to leave to the right *)
(* of the endpoint *)
PROCEDURE EndPointDir(READONLY end, prev : GridPoint.T) : Dir;

(* return TRUE if a neighbor exists in that dir, with the neighbor in res *)
PROCEDURE Neighbor(READONLY a : GridPoint.T; dir : Dir; 
                   VAR res : GridPoint.T) : BOOLEAN;

END EndPointStatus.
