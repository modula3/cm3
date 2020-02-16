(* $Id$ *)

MODULE Step;
IMPORT GridPoint, GridPointSet, GridPointCube;
FROM GridPoint IMPORT Layer;
IMPORT Cost;
FROM Cost IMPORT costs;
IMPORT Filled;
IMPORT RouteState;
IMPORT Fmt, Debug;
IMPORT Compass;
IMPORT ForbiddenSteps;
IMPORT RouteID;

(* note carefully:
   for ripping up, we use a special trick so we don't have to compute
   the cost for ripping up (which might involve table lookups, etc.)
   too often.  We return a special placeholder record that is used to
   generate the actual rip-up record when it comes to the head of the
   queue.  For this to work, the cost attached to the placeholder
   record must be less than or equal to the minimum rip-up cost.

 *)

CONST OptPts = 3;
CONST MaxNeighbors = NUMBER(Compass.Dir) + 2 + OptPts; (* max. no. of neighbors a pt. can have *)

VAR DoDebug := Debug.DebugThis("Step");

(* return value is new idx *)
(* may change stateArr even if it fails *)

(*
   This routine generates candidate movement points in the plane to get
   from GridPoint.T from to its NEWS neighbors.

   There is no guarantee that it is in fact OK to move to the neighbors.
*)
PROCEDURE MoveInPlane(READONLY from : GridPoint.T; 
                      dir: Compass.Dir; 
                      s : RouteState.T; 
                      VAR stateArr : ARRAY OF T;
                      idx : CARDINAL) : CARDINAL =
  VAR
    stepCost := SingleCostInPlane(from,dir);
    to := from;
  BEGIN
    <* ASSERT s # NIL *>
    WITH res = stateArr[idx] DO
      res.p := to;
      INC(res.p.x, Compass.Step[dir].x);
      INC(res.p.y, Compass.Step[dir].y);
      res.cost := stepCost;
      res.s := s;
      INC(idx)
    END;
    RETURN idx (* either idx' or idx' + 1 or idx' + 2 *)
  END MoveInPlane;

REVEAL
  Iterator = PublicIter BRANDED OBJECT 
    array : ARRAY [0..MaxNeighbors-1] OF T;
    i, n : INTEGER := -1;
  OVERRIDES
    next := IterNext;
  END;
        
PROCEDURE IterNext(self : Iterator; VAR s : T) : BOOLEAN =
  BEGIN
    IF self.i = -1 THEN 
      <* ASSERT FALSE *> 
    ELSIF self.i = self.n THEN
      RETURN FALSE
    ELSE
      s := self.array[self.i];
      INC(self.i);
      RETURN TRUE
    END
  END IterNext;

PROCEDURE Neighbors(READONLY p : GridPoint.T; 
                    <*UNUSED*>READONLY dst : GridPoint.T; 
                    nonObstacles, pointsThatCostZero : GridPointSet.T;
                    s : RouteState.T;
                    forbidden : ForbiddenSteps.T;
                    VAR iter : Iterator;
                    READONLY absMaxBBox : GridPointCube.T;
                    respectAbsMaxBBox : BOOLEAN) =
  VAR
    h := p.x; v := p.y; l := p.l;
    idx := 0;
    jdx : CARDINAL;
    res : ARRAY [0..MaxNeighbors-1] OF T;
  BEGIN
    IF iter = NIL THEN iter := NEW(Iterator) END;

    IF DoDebug THEN
      Debug.Out("Step.Neighbors( " & GridPoint.Format(p) & " )")
    END;

    (* find neighboring points and costs of moving *)
    (* these are the basic ones -- if we overstep boundaries or *)
    (* need to rip up, that comes later *)
    
    FOR i := FIRST(Compass.Dir) TO LAST(Compass.Dir) DO
      idx := MoveInPlane(p, i, s, res, idx)
    END;

    IF DoDebug THEN
      Debug.Out("Neighbors: idx = " & Fmt.Int(idx))
    END;

    (* vertical steps *)
    
    IF l # FIRST(Layer) THEN
      res[idx].p := 
          GridPoint.T { h, v, l - 1 }; (* down *)
      res[idx].s := s;
      res[idx].cost := SingleCost(p,res[idx].p);
      INC(idx)
    END;

    IF l # LAST(Layer) THEN
      res[idx].p := 
          GridPoint.T { h, v, l + 1 };  (* up *)
      res[idx].s := s;
      res[idx].cost := SingleCost(p,res[idx].p);
      INC(idx)
    END;

    jdx := idx;

    (*************** DONE MODIFYING idx ***************)

    (* if we are on the edge, we may need to add points in the *)
    (* neighboring cell *)
    (* note that we are only allowed to grow a cell by ONE UNIT PER PATH *)

    (* look for blockages *)
    FOR i := 0 TO idx - 1 DO

      (* this needs to get a bit trickier if we are expanding the
         current search cell ... *)

      (* we also need to make sure that we don't under-cut the CURRENT
         search path.. *)
      VAR 
        oldRte : RouteID.T;
        marked := NOT nonObstacles.member(res[i].p) AND
                      IsBlocked(res[i].p, s, oldRte);
        forbiddenStep := forbidden # NIL AND (forbidden.isForbidden(p, res[i].p) OR forbidden.isForbidden(res[i].p,p));
        outsideBbox := respectAbsMaxBBox AND 
                             NOT GridPointCube.Inside(absMaxBBox, res[i].p);
      BEGIN
        (* here we check for zero-cost, rip-ups and "infinite cost" actions *)
        (* we mark a future rip-up if that is warranted, else
           we delete the entry from the list of neighbors
           entirely 

           rip-up is OK if:
           ================           
           DoSpecials is TRUE
           MinRipUpCost is less than infinity

           rip-up is needed if:
           ====================           
           the neighbor is already marked by another route
           that route is non-NIL
           and that route is not yet ripped up
           and the stop is not forbidden for other reasons.

           rip-up is not needed if:
           ========================
           it's already ripped up

           Otherwise, if there is something in the way or the step is 
           forbidden, or if it costs too much, delete it!
        *)
        IF outsideBbox THEN
          res[i].cost := Cost.Infty; 
          DEC(jdx)
        ELSIF pointsThatCostZero # NIL AND pointsThatCostZero.member(res[i].p) 
          THEN
           IF forbiddenStep THEN
             res[i].cost := Cost.Infty; 
             DEC(jdx)
           ELSE
             res[i].cost := 0 (* woo woo *)
           END
        ELSIF DoSpecials AND Cost.MinRipUpCost < Cost.Infty AND marked AND 
          oldRte # RouteID.Nil AND
          (s = NIL OR NOT s.rippedUpAnywhere(oldRte)) AND NOT forbiddenStep THEN
          INC(res[i].cost,Cost.MinRipUpCost );

          (* cost of ripping up added via RipUp.Cost (does that work right?) *)
          res[i].type := Type.RipUp;
          
        ELSIF marked AND 
              (oldRte # RouteID.Nil AND 
               s # NIL AND 
               s.rippedUpAnywhere(oldRte)) THEN
          (* skip: we're good; nothing needed *)

        ELSIF marked OR res[i].cost >= Cost.Infty OR forbiddenStep THEN 
          res[i].cost := Cost.Infty; 
          DEC(jdx)

     (* ELSE SKIP --- all other cases *)
        END
      END
    END;

    (* XXX we no longer fiddle with internals of res[] below this *)

    (* re-alloc in proper sized array *)

    VAR
      j := 0;
    BEGIN
      FOR i := 0 TO idx - 1 DO
        IF res[i].cost # Cost.Infty THEN
          iter.array[j] := res[i];
          INC(j)
        END
      END;
      <* ASSERT j = jdx *>
      iter.n := j;
      iter.i := 0;
      IF DoDebug THEN
        Debug.Out("Step.Neighbors: " & GridPoint.Format(p) & ":");
        FOR i := 0 TO iter.n - 1 DO
          Debug.Out("Neighbor " & Fmt.Int(i) & ": " & 
            GridPoint.Format(iter.array[i].p) & 
            " cost = " & Fmt.Int(iter.array[i].cost) & 
            " type?=RipUp = " & Fmt.Bool(iter.array[i].type = Type.RipUp))
        END
      END;
      RETURN
    END
  END Neighbors;

 
(* is a given point already blocked---and not already ripped up in the given
   state? *)
PROCEDURE IsBlocked(READONLY p : GridPoint.T; s : RouteState.T; VAR by : RouteID.T) : BOOLEAN =
  VAR
    marked := Filled.Marked(p); 
    blockage : RouteID.T;
  BEGIN
    IF marked THEN blockage := Filled.MarkedRoute(p) END;
    
    (* if it's ripped up it's not really blocked, dummy *)
    IF s # NIL AND s.rippedUpHere(blockage,p) THEN marked := FALSE END;

    IF marked THEN by := blockage END;

    RETURN marked
  END IsBlocked;

PROCEDURE SingleCostInPlane(from : GridPoint.T; dir : Compass.Dir) : INTEGER =
  BEGIN
    WITH l = from.l DO
      CASE dir OF 
      | Compass.Dir.N, Compass.Dir.S => RETURN costs()[l].yCost
      | Compass.Dir.E, Compass.Dir.W => RETURN costs()[l].xCost
      END
    END
  END SingleCostInPlane;
    
(* cost between neighbors --- this routine is only used vertically now *)
PROCEDURE SingleCost(from, to : GridPoint.T) : INTEGER =
  BEGIN

    WITH deltax = ABS(from.x - to.x), 
         deltay = ABS(from.y - to.y), 
         deltal = ABS(from.l - to.l) DO
      
      <* ASSERT deltax <= 1 *>
      <* ASSERT deltay <= 1 *>
      <* ASSERT deltal <= 1 *>
      <* ASSERT deltax + deltay + deltal = 1 *>
    END;

    IF from.x # to.x THEN
      RETURN costs()[from.l].xCost
    ELSIF from.y # to.y THEN
      RETURN costs()[from.l].yCost
    ELSIF to.l > from.l THEN
      RETURN costs()[from.l].upCost
    ELSIF to.l < from.l THEN
      RETURN costs()[to.l].upCost
    ELSE
      <* ASSERT FALSE *>
    END
  END SingleCost;

BEGIN DoSpecials := TRUE END Step.

