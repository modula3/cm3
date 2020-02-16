(* $Id$ *)

INTERFACE Cost;
IMPORT GridPoint;
FROM GridPoint IMPORT Layer;
IMPORT CostRec;

TYPE
  Def = RECORD 
    layer : Layer;             (* number of this layer (not needed)    *)
    xCost, yCost : INTEGER;    (* cost to move one step in x and y     *)
    upCost : INTEGER;          (* cost for a contact to the next layer *)
  END;

CONST
  Infty = LAST(INTEGER) DIV 8;

(********** BASIC COSTS **********)

(* we have made the costs themselves settable.  The ripup costs below
   are not yet settable *)

TYPE
  Costs = ARRAY Layer OF Def;

CONST
  DefaultCosts = Costs {
    Def { 0, 999, 999,  Infty },
    Def { 1, 50, 15,  100 },
    Def { 2, 10, 100, 100 },
    Def { 3, 100, 10, 100 },
    Def { 4, 5, 100,  50 },
    Def { 5, 100, 5,  Infty (* ignored *)}
    };

PROCEDURE costs() : Costs;
PROCEDURE Set_costs(READONLY newCosts : Costs);

(* is it OK to move more than one step vertically? *)
PROCEDURE MoveMoreThanOneVertically() : BOOLEAN;

PROCEDURE SetMoveMoreThanOneVertically(to : BOOLEAN);

(********** RIP UP AND REROUTE **********)

VAR  MinRipUpCost : CARDINAL; (* minimum increment for rip-up-and-reroute *)
CONST DefMinRipUpCost = 500;

VAR  RipUpIncrement : REAL; (* how much more does it cost to rip up a path than
                         it originally cost? *)
CONST DefRipUpIncrement = 2.0;

VAR  RipUpMultiplier : REAL; (* how much more expensive is a ripped-up path
                                than last time? *)
CONST DefRipUpMultiplier = 2.0;

(********** EXTRA COST FOR MOVING OUTSIDE B-BOX OF SRC AND TGT **********)

(* this variable says how much more it should cost to route outside the
   bounding box made up by the source and target points.

   This is essentially a hack to prevent ripup and reroute from eating
   too much memory.

   0.1 means that the cost will be twice as high to route 10 steps outside
   the bbox as it was to route inside it.
*)

VAR  OutOfBoundsIncreaseRate : REAL;
CONST DefOutOfBoundsIncreaseRate = 0.01;

VAR (* CONST *) bestXcost, bestYcost : INTEGER; 

(********** TOTAL COST CUTOFFS **********)

(* N.B. max cost allowed at step is computed with:
   MIN(MaxMaxCost, greedyCost * MaxCostOverGreedy + BaseCost)
*)

VAR  MaxMaxCost : CARDINAL; (* no paths allowed that are more expensive *)
CONST DefMaxMaxCost = 100000; 


VAR  BaseCost : CARDINAL; (* gets added to max cost allowed *)
CONST DefBaseCost = 5000;

VAR  MaxCostOverGreedy : REAL ; (* no paths allowed with this much increment *)
CONST DefMaxCostOverGreedy = 10.0;
  
(* compute the cost of stepping from a point to one of his neighbors *)
(* WARNING: for efficiency, this routine does no error checking on   *)
(* arguments.  You will get the Wrong Answer if from and to are not  *)
(* neighbors. *)
(* this should be folded in with the GridPoint.Neighbors *)
(* costs and neighbors are part of the same calculation *)

PROCEDURE Neighbors(from, to : GridPoint.T) : INTEGER;

PROCEDURE GetRec(READONLY p, tgt : GridPoint.T) : CostRec.T;

EXCEPTION OutOfMemory;
PROCEDURE Mark(p, previous, tgt : GridPoint.T; d : INTEGER) RAISES { OutOfMemory};

PROCEDURE InitRoute(memLimit : CARDINAL := 0); (* call before routing;
                                                  set memLimit to max. # of
                                                  points to remember *)

PROCEDURE Greedy(READONLY src, dst : GridPoint.T) : INTEGER;

VAR costRecSizeHWM : CARDINAL;    (* cleared on init, max of all sizes.
                                     (normally there are two tables) *)

END Cost.







